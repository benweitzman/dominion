import Dominion
import Dominion.Cards
import Dominion.Strategies

main = dominionWithOpts [Cards [smithy], Iterations 1000, Log False] ["adit" `uses` bigMoneySmithy, "maggie" `uses` bigMoney2]
