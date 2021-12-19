import module1.functions.p

import scala.util.matching.Regex
import module1.{list, opt}
import module1.list._
val isCyrillic = "['\\p{IsCyrillic}]*".r

isCyrillic.matches("Hello")
val option = opt.Option.Some("test")
option.printIfAny
val newV = option.filter(p => p.isEmpty)
option.zip(opt.Option.Some("test1"))

//List
val l1 = List.apply(1,2,3,4,5)
val l2 = List.apply(1,2,3,4,7)
l1.map(x => x + 3)
l1.mkString('!')
l2.filter(p => p > 3)
l2.reverse
l2.::(7)
incList(l2)
shoutString(List("Hello", "my", "first", "home", "work")).mkString()