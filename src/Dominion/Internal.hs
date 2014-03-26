module Dominion.Internal (

  -- | Note: You shouldn't need to import this module...the
  -- interesting functions are re-exported by the Dominion module.
  --
  -- Use any other functions in here at your own risk.
  module Dominion.Internal
) where
import           Control.Applicative
import           Control.Arrow
import           Control.Lens        hiding (has, indices)
import           Control.Monad       (liftM)
import           Control.Monad.State hiding (state)
import           Data.List
import           Data.Map.Lazy       ((!))
import qualified Data.Map.Lazy       as M
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import qualified Dominion.Types      as T
import           Dominion.Utils
import           Prelude             hiding (log)
import           System.IO.Unsafe
import           Text.Printf

-- | see all of the cards in a player's hand.
--
-- > cards <- currentHand playerId
currentHand :: T.PlayerId -> T.Dominion [T.CardWrap]
currentHand playerId = T.hand <$> getPlayer playerId

-- | see if a player has a card in his hand.
--
-- > hasCard <- playerId `has` chapel
has :: T.PlayerId -> T.CardWrap -> T.Dominion Bool
has playerId card = do
    player <- getPlayer playerId
    return $ card `elem` T.hand player

-- | see how many of this card a player has.
--
-- > numMarkets <- countNum playerId market
countNum :: T.PlayerId -> T.CardWrap -> T.Dominion Int
countNum playerId card = do
    player <- getPlayer playerId
    let allCards = T.deck player ++ T.discard player ++ T.hand player
    return $ count card allCards

-- | What this card is worth in money.
coinValue :: T.Card a => a -> Int
coinValue = T.coinValue

-- | Get the current round number.
getRound :: T.Dominion Int
getRound = T.roundNum <$> get

-- | How much money this player's hand is worth (also counts any money you
-- get from action cards, like +1 from market).
handValue :: T.PlayerId -> T.Dominion Int
handValue playerId = do
    player <- getPlayer playerId
    return $ sum (map coinValue (T.hand player)) + T.extraMoney player

-- | Check if this card's pile is empty. Returns True is the card is not in play.
pileEmpty :: T.CardWrap -> T.Dominion Bool
pileEmpty card = do
    state <- get
    return $ case M.lookup card (T.cards state) of
      Nothing -> True
      Just x -> x == 0

-- | Returns the card, or Nothing if that pile is empty.
-- Useful because it automatically checks whether the pile is empty, and
-- modifies state to subtract a card from the pile correctly.
getCard :: T.CardWrap -> T.Dominion (Maybe T.CardWrap)
getCard card = do
    empty <- pileEmpty card
    if empty
      then return Nothing
      else do
        -- modify $ over T.cards (decrement card)
        modify $ \s -> s{T.cards=(decrement card (T.cards s))}
        return $ Just card

-- | Convenience function. Prints out a line if verbose, AND prints out
-- info about the related player...name, money, # of buys, # of actions.
log :: T.PlayerId -> String -> T.Dominion ()
log playerId str = do
    player <- getPlayer playerId
    money <- handValue playerId
    let name = T.playerName player
        buys = T.buys player
        actions = T.actions player
        statusLine = printf "[player %s, name: %s, money: %s, buys: %s, actions: %s]" (yellow . show $ playerId) (yellow name) (green . show $ money) (green . show $ buys) (red . show $ actions)
    log_ $ statusLine ++ ": " ++ green str

-- | Like `log` but doesn't print out info about a player
log_ :: String -> T.Dominion ()
log_ str = do
    state <- get
    when (T.verbose state) $ liftIO . putStrLn $ str

-- | Given a player id and a number of cards to draw, draws that many cards
-- from the deck, shuffling if necessary.
drawFromDeck :: T.PlayerId -> Int -> T.Dominion [T.CardWrap]
drawFromDeck playerId numCards = do
    player <- getPlayer playerId
    let deck = T.deck player
    if length deck >= numCards
      then draw numCards
      else do
        let inDeck = length deck
        lastCards <- draw inDeck
        shuffleDeck playerId
        liftM (++ lastCards) $ draw (numCards - inDeck)
 where
   draw numCards = do
       player <- getPlayer playerId
       let drawnCards = take numCards (T.deck player)
       modifyPlayer playerId $ (\p -> p{T.deck=drop numCards (T.deck p),T.hand=(T.hand p) ++ drawnCards})
       return drawnCards

-- | Like `modify` for the `State` monad, but works on players.
-- Takes a player id and a function that modifies the player.
modifyPlayer :: T.PlayerId -> (T.Player -> T.Player) -> T.Dominion ()
modifyPlayer playerId func = modify $ \s -> s{T.players=modifyListIndex playerId func (T.players s)}

-- | Like `modifyPlayer`, but modifies every player *except* the one specified with the player id.
modifyOtherPlayers :: T.PlayerId -> (T.Player -> T.Player) -> T.Dominion ()
modifyOtherPlayers playerId func = do
    state <- get
    let players = indices (T.players state) \\ [playerId]
    forM_ players $ \pid -> modifyPlayer pid func

setupForTurn :: T.PlayerId -> T.Dominion ()
setupForTurn playerId = do
    drawFromDeck playerId 5
    modifyPlayer playerId (\s -> s{T.actions=1,T.buys=1,T.extraMoney=0})

playTurn :: T.PlayerId -> T.Strategy -> T.Dominion ()
playTurn playerId strategy = do
    roundNum <- getRound
    when (roundNum == 1) $ setupForTurn playerId
    player <- getPlayer playerId
    log playerId $ "player's hand has: " ++ (show . map T.name $ T.hand player)
    strategy playerId
    discardHand playerId
    -- we draw from deck *after* to set up the next hand NOW,
    -- instead of calling this at the beginning of the function.
    -- The reason is, if someone else plays a militia, or a council room,
    -- these players need to be able to modify their deck accordingly
    -- even if its not their turn.
    setupForTurn playerId

isAction card = T.Action `elem` (T.types card)
isAttack card = T.Attack `elem` (T.types card)
isReaction card = T.Reaction `elem` (T.types card)
isTreasure card = T.Treasure `elem` (T.types card)
isVictory card = T.Victory `elem` (T.types card)

{- countPoints :: T.PlayerId -> Dominion Int
countPoints playerId = sum $ map countValue effects
    where cards        = player ^. T.deck ++ player ^. T.discard ++ player ^. T.hand
          victoryCards = filter isVictory cards
          effects      = concatMap T.types victoryCards
          countValue (T.VPValue x) = x
          countValue (T.GardensEffect) = length cards `div` 10
          countValue _ = 0 -}

countPoints :: T.PlayerId -> T.Dominion Int
countPoints playerId = do
  player <- getPlayer playerId
  let cards = concat $ [T.deck, T.discard, T.hand] <*> pure player
  foldM (\accum card -> do x <- T.points playerId card
                           return (x + accum)) 0 cards

-- | Get player from game state specified by this id.
-- This is useful sometimes:
--
-- > import qualified Dominion.Types as T
-- > import Control.Lens
-- >
-- > player <- getPlayer playerId
-- >
-- > -- How many buys does this player have?
-- > player ^. T.buys
-- >
-- > -- How many actions does this player have?
-- > player ^. T.actions
getPlayer :: T.PlayerId -> T.Dominion T.Player
getPlayer playerId = do
    state <- get
    return $ (T.players state) !! playerId

-- | Convenience function. @ 4 \`cardsOf\` estate @ is the same as @ take 4 . repeat $ estate @
cardsOf = replicate 

-- | Move this players discards + hand into his deck and shuffle the deck.
shuffleDeck :: T.PlayerId -> T.Dominion ()
shuffleDeck playerId = do player <- getPlayer playerId
                          shuffleFunction <- liftIO $ shuffleDeck_ player
                          modifyPlayer playerId shuffleFunction

shuffleDeck_ :: T.Player -> IO (T.Player -> T.Player)
shuffleDeck_ player = do newDeck <- deckShuffle (deck ++ discard ++ hand)
                         return $ const player{T.discard=[], T.deck=newDeck}
          where discard = T.discard player
                deck    = T.deck player
                hand    = T.hand player

-- | Check that this player is able to purchase this card. Returns
-- a `Right` if they can purchase the card, otherwise returns a `Left` with
-- the reason why they can't purchase it.
validateBuy :: T.PlayerId -> T.CardWrap -> T.Dominion T.PlayResult
validateBuy playerId card = do
    money <- handValue playerId
    state <- get
    player <- getPlayer playerId
    cardGone <- pileEmpty card
    return . getFirst . mconcat . map First $
      [failIf (money < (T.cost card)) $ printf "Not enough money. You have %d but this card costs %d" money (T.cost card)
      ,failIf cardGone $ printf "We've run out of that card (%s)" (T.name card)
      ,failIf ((T.buys player) < 1) "You don't have any buys remaining!"
      ]

-- | Check that this player is able to play this card. Returns
-- a `Right` if they can play the card, otherwise returns a `Left` with
-- the reason why they can't play it.
validatePlay :: T.PlayerId -> T.CardWrap -> T.Dominion T.PlayResult
validatePlay playerId card = do
    player <- getPlayer playerId
    log playerId $ printf "validating that %s has a %s" (T.playerName player) (T.name card)
    return . getFirst . mconcat . map First $
      [failIf (T.actions player < 1) "You don't have any actions remaining!"
      ,failIf (card `notElem` (T.hand player)) $ printf
        "You can't play a %s because you don't have it in your hand!" (T.name card)
      ]

-- Discard this player's hand.
discardHand :: T.PlayerId -> T.Dominion ()
discardHand playerId = modifyPlayer playerId $ \player -> player{T.hand=[],T.discard=T.discard player ++ T.hand player}

-- for parsing options
findIteration :: [T.Option] -> Maybe Int
findIteration [] = Nothing
findIteration (T.Iterations x : xs) = Just x
findIteration (_:xs) = findIteration xs

-- for parsing options
findLog :: [T.Option] -> Maybe Bool
findLog [] = Nothing
findLog (T.Log x : xs) = Just x
findLog (_:xs) = findLog xs

-- for parsing options
findCards :: [T.Option] -> Maybe [T.CardWrap]
findCards [] = Nothing
findCards (T.Cards x : xs) = Just x
findCards (_:xs) = findCards xs

-- | Keep drawing a card until the provided function returns true.
-- The function gets a list of the cards drawn so far,
-- most recent first. Returns a list of all the cards drawn (these cards
-- are also placed into the player's hand)
drawsUntil :: T.PlayerId -> ([T.CardWrap] -> T.Dominion Bool) -> T.Dominion [T.CardWrap]
drawsUntil = drawsUntil_ []

-- internal use for drawsUntil
drawsUntil_ :: [T.CardWrap] -> T.PlayerId -> ([T.CardWrap] -> T.Dominion Bool) -> T.Dominion [T.CardWrap]
drawsUntil_ alreadyDrawn playerId func = do
    drawnCards <- drawFromDeck playerId 1
    let cards = drawnCards ++ alreadyDrawn
    stopDrawing <- func cards
    if stopDrawing
      then return cards
      else drawsUntil_ cards playerId func

-- Does this card say you trash it when you play it?
trashThisCard :: T.Card a => a->  Bool
trashThisCard card = False --T.TrashThisCard `elem` (card ^. T.effects)

-- | Player trashes the given card.
trashesCard :: T.PlayerId -> T.CardWrap -> T.Dominion ()
playerId `trashesCard` card = do
  hasCard <- playerId `has` card
  when hasCard $ modifyPlayer playerId (\p -> p{T.hand=delete card (T.hand p)})

-- | Player discards the given card.
discardsCard :: T.PlayerId -> T.CardWrap -> T.Dominion ()
playerId `discardsCard` card = do
  hasCard <- playerId `has` card
  when hasCard $ modifyPlayer playerId $ (\p -> p{T.hand=delete card (T.hand p),T.discard=card:T.discard p})

-- Player returns the given card to the top of their deck.
returnsCard :: T.PlayerId -> T.CardWrap -> T.Dominion ()
playerId `returnsCard` card = do
  hasCard <- playerId `has` card
  when hasCard $ modifyPlayer playerId $ (\p -> p{T.hand=delete card (T.hand p), T.deck=card:T.deck p})

-- If the top card in the player's deck is one of the cards
-- listed in the provided array, then discard that card (used with spy).
discardTopCard :: [T.CardWrap] -> T.Player -> T.Player
discardTopCard cards player = if topCard `elem` cards
                                then player{T.deck=tail deck,T.discard=topCard:T.discard player}
                                else player
    where topCard = head $ T.deck player
          deck = T.deck player

-- TODO reaction
-- If this player has a victory card in his/her hand,
-- it is put on top of their deck *unless* they have a moat in their hand.
-- Used with militia.
returnVPCard :: T.Player -> T.Player
returnVPCard player = let hand = T.hand player
                          victoryCards = filter isVictory hand
                          card = head victoryCards
                      in player{T.hand=delete card (T.hand player),T.deck=card:T.deck player}

-- TODO how do they choose what to discard??
-- Right now I'm just choosing to discard the least expensive.
-- | Player discards down to x cards.
discardsTo :: T.Player -> Int -> T.Player
player `discardsTo` x = player{T.hand=toKeep,T.discard=T.discard player ++ toDiscard}
    where hand = sortBy (comparing T.cost) $ T.hand player
          toDiscard = take (length hand - x) hand
          toKeep = hand \\ toDiscard

{-- | Used internally by the `plays` function. Each card has a list of
-- effects (like smithy has `PlusCard 3`). This function applies the given
-- effect. It returns `Nothing` if the effect doesn't need a `Followup`,
-- or it returns a `Just Followup`.
usesEffect :: T.PlayerId -> T.CardEffect -> T.Dominion (Maybe T.Followup)
playerId `usesEffect` (T.PlusAction x) = do
    log playerId ("+ " ++ show x ++ " actions")
    modifyPlayer playerId $ over T.actions (+x)
    return Nothing

playerId `usesEffect` (T.PlusCoin x) = do
    log playerId ("+ " ++ show x ++ " coin")
    modifyPlayer playerId $ over T.extraMoney (+x)
    return Nothing

playerId `usesEffect` (T.PlusBuy x) = do
    log playerId ("+ " ++ show x ++ " buys")
    modifyPlayer playerId $ over T.buys (+x)
    return Nothing

playerId `usesEffect` (T.PlusCard x) = do
    log playerId ("+ " ++ show x ++ " cards")
    drawFromDeck playerId x
    return Nothing

playerId `usesEffect` effect@(T.PlayActionCard x) = return $ Just (playerId, effect)

playerId `usesEffect` (T.AdventurerEffect) = do
    log playerId "finding the next two treasures from your deck..."
    drawnCards <- playerId `drawsUntil` (\cards -> return $ countBy isTreasure cards == 2)
    -- the cards that weren't treasures need to be discarded
    forM_ (filter (not . isTreasure) drawnCards) $ \card -> playerId `discardsCard` card
    return Nothing

playerId `usesEffect` (T.BureaucratEffect) = do
    card_ <- getCard CA.silver
    case card_ of
      Nothing -> return ()
      Just card -> do
        log playerId "+ silver"
        modifyPlayer playerId $ over T.deck (card:)
    modifyOtherPlayers playerId returnVPCard
    return Nothing

playerId `usesEffect` T.CellarEffect = return $ Just (playerId, T.CellarEffect)

playerId `usesEffect` T.ChancellorEffect = return $ Just (playerId, T.ChancellorEffect)

playerId `usesEffect` effect@(T.TrashCards x) = do
    log playerId ("Trash up to " ++ show x ++ " cards from your hand.")
    return $ Just (playerId, effect)

playerId `usesEffect` effect@(T.OthersPlusCard x) = do
    log playerId ("Every other player draws " ++ show x ++ " card.")
    state <- get
    let players = indices (state ^. T.players) \\ [playerId]
    forM_ players $ \pid -> drawFromDeck pid 1
    return Nothing

playerId `usesEffect` effect@(T.GainCardUpto x) = do
    log playerId ("Gain a card costing up to " ++ show x ++ " coins.")
    return $ Just (playerId, effect)

-- TODO this doesn't set aside any action cards.
-- How do I implement the logic for choosing that?
-- Basically it allows the player to go through
-- and choose the action card they want?
playerId `usesEffect` effect@T.LibraryEffect = do
    log playerId "Drawing to 7 cards..."
    drawsUntil playerId $ \_ -> do
                 player <- getPlayer playerId
                 return $ length (player ^. T.hand) == 7
    return Nothing

-- NOTE: one side effect of this + council room is:
-- every player needs to draw their next hand immediately
-- after they finish playing, instead of at the start of when
-- they play. Otherwise suppose someone plays a council room
-- followed by a militia. I need to codify that properly.
playerId `usesEffect` effect@(T.OthersDiscardTo x) = do
    log playerId ("Every other player discards down to " ++ show x ++ " cards.")
    modifyOtherPlayers playerId (`discardsTo` x)
    return Nothing

playerId `usesEffect` T.MineEffect = return $ Just (playerId, T.MineEffect)

playerId `usesEffect` T.MoneylenderEffect = do
    hasCard <- playerId `has` CA.copper
    when hasCard $ do
      log playerId "Trashing a copper. +3 coin"
      playerId `trashesCard` CA.copper
      modifyPlayer playerId $ over T.extraMoney (+3)
    return Nothing

playerId `usesEffect` T.RemodelEffect = return $ Just (playerId, T.RemodelEffect)

playerId `usesEffect` T.SpyEffect = return $ Just (playerId, T.SpyEffect)

playerId `usesEffect` T.ThiefEffect = return $ Just (playerId, T.ThiefEffect)

playerId `usesEffect` (T.OthersGainCurse x) = do
    log playerId ("All other players gain " ++ show x ++ " curses.")
    let card = CA.curse
    empty <- pileEmpty card
    if empty
      then return Nothing
      else do
        modifyOtherPlayers playerId (over T.discard (card:))
        state <- get
        times (length (state ^. T.players) - 1) $ do
          modify $ over T.cards (decrement card)
          return ()
        return Nothing

-- only counted at the end of the game.
playerId `usesEffect` T.GardensEffect = return Nothing
playerId `usesEffect` _ = return Nothing
-}

-- Checks that the player can gain the given card, then adds it to his/her
-- discard pile.
gainCardUpTo :: T.PlayerId -> Int -> T.CardWrap -> T.Dominion T.PlayResult
gainCardUpTo playerId value card =
  if (T.cost card) > value
    then return . Just $ printf "Card is too expensive. You can gain a card costing up to %d but this card costs %d" value (T.cost card)
    else do
      result <- getCard card
      case result of
        Nothing -> return . Just $ printf "We've run out of that card (%s)" (T.name card)
        (Just card_) -> do
          modifyPlayer playerId (\p -> p{T.discard=card : T.discard p})
          return Nothing

---------------------
-- Basic Card Actions
---------------------

noop :: T.Virtual
noop = T.virtual

vpValue :: Int -> T.Virtual
vpValue n = noop{T.pointsFunction=const $ return n }

validator :: T.CardWrap -> T.Virtual
validator card = noop{T.playFunction = \pid -> do result <- validatePlay pid card
                                                  case result of
                                                    Just x -> return (Just x)
                                                    Nothing -> do log pid $ printf "plays a %s!" (T.name card)
                                                                  return Nothing
                     }

trasher :: T.CardWrap -> T.Virtual
trasher card = noop{T.playFunction = \pid -> do trashesCard pid card 
                                                return Nothing
                   }

discarder :: T.CardWrap -> T.Virtual
discarder card = noop{T.playFunction = \pid -> do discardsCard pid card 
                                                  return Nothing
                     }

plusCards :: Int -> T.Virtual
plusCards n = noop{T.playFunction = \pid -> do drawFromDeck pid n
                                               return Nothing
                  }

{--mkCard :: String -> Int -> Int -> [T.CardType] -> T.Virtual -> T.Card
mkCard name cost worth types effects = 
    let card = T.Card name cost worth types (validator card) effects (discarder card)
    in card

mkCardToTrash :: String -> Int -> Int -> [T.CardType] -> T.Virtual -> T.Card
mkCardToTrash name cost worth types effects = 
    let card = T.Card name cost worth types (validator card) effects (trasher card)
    in card--}
