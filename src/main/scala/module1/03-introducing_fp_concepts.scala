package module1

import scala.annotation.tailrec
import scala.language.postfixOps



/**
 * referential transparency
 */
 object referential_transparency{


  case class Abiturient(id: String, email: String, fio: String)

  type Html = String

  sealed trait Notification

  object Notification{
    case class Email(email: String, text: Html) extends Notification
    case class Sms(telephone: String, msg: String) extends Notification
  }


  case class AbiturientDTO(email: String, fio: String, password: String)

  trait NotificationService{
    def sendNotification(notification: Notification): Unit
    def createNotification(abiturient: Abiturient): Notification
  }

  trait AbiturientService{

    def registerAbiturient(abiturientDTO: AbiturientDTO, uuid: String): Abiturient
  }

  class AbiturientServiceImpl(notificationService: NotificationService) extends AbiturientService{

    override def registerAbiturient(abiturientDTO: AbiturientDTO, uuid: String): Abiturient = {
      val abiturient = Abiturient(uuid, abiturientDTO.email, abiturientDTO.fio)
      notificationService.sendNotification(Notification.Email(abiturient.email, "Some message"))
      abiturient
    }

  }
}


 // recursion

object recursion {

  /**
   * Реализовать метод вычисления n!
   * n! = 1 * 2 * ... n
   */

   def fact(n: Int): Int = {
       var _n = 1
       var i = 2
       while(i <= n){
           _n *=  i
           i += 1
       }
       _n
   }


   def factRec(n: Int): Int = {
    if (n<=0) 1 else n * factRec(n-1) 
   }

 
   def factTailRec(n: Int): Int = {
       @tailrec
       def loop(n: Int, accum: Int): Int = 
           if(n == 1) accum
           else loop(n - 1, n * accum)
        loop(n, 1)
   }

  


  /**
   * реализовать вычисление N числа Фибоначчи
   * F0 = 0, F1 = 1, Fn = Fn-1 + Fn - 2
   *
   */
  def fibonacсi(n: Int): Int = {
    if (n == 1 || n == 2 ) 1
    else fibonacсi(n-1) + fibonacсi(n-2)
  }

}

object hof{

   trait Consumer{
       def subscribe(topic: String): LazyList[Record]
   }

   case class Record(value: String)

   case class Request()
   
   object Request {
       def parse(str: String): Request = ???
   }

   def createRequestSubscription() = {
       val cons: Consumer = ???

       val stream: LazyList[Record] = cons.subscribe("request")

       stream.foreach{ rec =>
            val req: Request = Request.parse(rec.value)
            // save(request)
       }
   }

   def createSubscription[T](topic: String, action: Record => T): Unit = {
       val cons: Consumer = ???

       val stream: LazyList[Record] = cons.subscribe(topic)
       stream.foreach{ rec =>
            action(rec)
       }
   }
   def createRequestSubscription2 = createSubscription("request", r => {
       val req: Request = Request.parse(r.value)
            // save(request)
   })
  

  // обертки

  def logRunningTime[A, B](f: A => B): A => B = { a =>
        val start = System.currentTimeMillis()
        val result = f(a)
        val end = System.currentTimeMillis()
        println(end - start)
        result
  }






  // изменение поведения ф-ции

  val arr = Array(1, 2, 3, 4, 5)

  def isOdd(i: Int): Boolean = i % 2 > 0

  
   
  def not[A](f: A => Boolean): A => Boolean = a => ! f(a) 
  
  
  lazy val isEven = not(isOdd)




  // изменение самой функции

  // Follow type implementation

  def partial[A, B, C](a: A, f: (A, B) => C): B => C = b => f(a, b)

  def sum(x: Int, y: Int): Int = x + y

  val res: Int => Int = partial(1, sum)

  res(3) 


}






/**
 *  Реализуем тип Option
 */


 object opt {

  /**
   *
   * Реализовать тип Option, который будет указывать на присутствие либо отсутсвие результата
   */

   sealed trait Option[+T]{
       def isEmpty: Boolean = this match {
           case Option.Some(v) => false
           case Option.None => true
       }

       def get: T = this match {
           case Option.Some(v) => v
           case Option.None => throw new Exception("Get on empty option")
       }

       def map[B](f: T => B): Option[B] = this match {
           case Option.Some(v) => Option.Some(f(v))
           case Option.None => Option.None
       }

       def flatMap[B](f: T => Option[B]): Option[B] = this match {
           case Option.Some(v) => f(v)
           case Option.None => Option.None
       }

       def printIfAny: Unit = this match {
          case Option.Some(v) => println(v)
          case Option.None =>
        }

    def zip[B](that: Option[B]): Option[(T, B)] = (this, that) match{
      case (Option.Some(v1), Option.Some(v2)) => Option.Some(v1,v2)
      case (_, Option.None) => Option.None
      case (Option.None, _) => Option.None
    }

    def filter(p: T => Boolean): Option[T] = this match{
      case Option.Some(v) if p(v) => this
      case _ => Option.None
    }
   }

   object Option{
        case class Some[T](v: T) extends Option[T]
        case object None extends Option[Nothing]

        def apply[T](v: T): Option[T] = v match{
          case v if v != null => Some(v)
          case _ => None
        }

     /** Реализовать метод printIfAny, который будет печатать значение, если оно есть
      */

     def printIfAny[T](o: Option[T]): Unit = o.printIfAny

     /** Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
      */
     def zip[A, B](o1: Option[A], o2: Option[B]): Option[(A, B)] = o1 zip o2

     /** Реализовать метод filter, который будет возвращать не пустой Option
      * в случае если исходный не пуст и предикат от значения = true
      */
     def filter[A](o: Option[A], f: A => Boolean): Option[A] = o.filter(f)

   }

 }

 object list {
   /**
    *
    * Реализовать односвязанный иммутабельный список List
    * Список имеет два случая:
    * Nil - пустой список
    * Cons - непустой, содердит первый элемент (голову) и хвост (оставшийся список)
    */

    sealed trait List[+T]{

     def ::[A >: T](elem: A): List[A] = list.Cons(elem, this)

     def mkString (del: Char = ','): String = {
       @tailrec
       def loop(l: List[T], acc: String = "" ): String = l match{
         case list.Nil => acc.substring(1)
         case list.Cons(h,t) => loop(t, s"$acc$del$h")
       }
       loop(this)
     }

     def reverse: List[T] = {
       @tailrec
       def loop(l: List[T], acc: List[T] = list.Nil ): List[T] = l match{
         case list.Nil => acc
         case list.Cons(head, tail)  => loop(tail, head :: acc)
       }
       loop(this)
     }

     def map[B] (f: T => B): List[B] = {
       @tailrec
       def loop (l: List[T], acc: List[B] = Nil): List[B] = l match {
         case list.Nil => acc.reverse
         case list.Cons(h, t) => loop(t, f(h) :: acc)

       }
       loop(this)
     }

     def filter(f: T => Boolean):List[T] = {
       @tailrec
       def loop (l: List[T], acc: List[T] = Nil): List[T] = l match {
         case Cons(h, t) if f(h) => loop(t, h:: acc)
         case Cons(_, t) => loop(t, acc.reverse)
         case Nil => acc
       }
       loop(this).reverse
     }

    }

    case class Cons[A](head: A, tail: List[A]) extends List[A]
    case object Nil extends List[Nothing]

    object List {

      def apply[T](v: T*): List[T] = if (v.isEmpty) Nil else list.Cons(v.head, apply(v.tail: _*))
    }



    /**
     * Метод cons, добавляет элемент в голову списка, для этого метода можно воспользоваться названием `::`
     *
     */
    def ::[T](el: T, l: List[T] = list.Nil): List[T] = Cons(el, l)


    /**
      * Конструктор, позволяющий создать список из N - го числа аргументов
      * Для этого можно воспользоваться *
      *
      * Например вот этот метод принимает некую последовательность аргументов с типом Int и выводит их на печать
      * def printArgs(args: Int*) = args.foreach(println(_))
      */
    def * [T](args: T*): List[T] = {
      var l: List[T] = list.Nil
      args.foreach { a => l = ::(a, l) }
      l.reverse
    }


/** Метод mkString возвращает строковое представление списка, с учетом переданного разделителя
    */
   def mkString[T](l: List[T], delim: Char = ','): String = l.mkString(delim)

   /** Реализовать метод reverse который позволит заменить порядок элементов в списке на противоположный
    */
   def reverse[T](l: List[T]): List[T] = l.reverse

   /** Реализовать метод map для списка который будет применять некую ф-цию к элементам данного списка
    */
   def map[A, B](l: List[A])(f: A => B): List[B] = l.map(f)

   /** Реализовать метод filter для списка который будет фильтровать список по некому условию
    */
   def filter[T](l: List[T])(f: T => Boolean): List[T] = l.filter(f)

    /**
      *
      * Написать функцию incList котрая будет принимать список Int и возвращать список,
      * где каждый элемент будет увеличен на 1
      */
      def incList (list: List[Int]): List[Int] = list.map(_ + 1)

    /**
      *
      * Написать функцию shoutString котрая будет принимать список String и возвращать список,
      * где к каждому элементу будет добавлен префикс в виде '!'
      */

   def shoutString (list: List[String]): List[String] = list.map(_.prepended('!'))
 }
