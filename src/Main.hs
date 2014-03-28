import Dominion
import Dominion.Cards
import Dominion.Strategies

main = dominionWithOpts [Cards [smithy, throneroom], Iterations 1, Log True] [ "adit" `uses` bigMoney
                                                                             , "maggie" `uses` stupidStrategy
                                                                             , "bessie" `uses` stupidStrategy
                                                                             ]
