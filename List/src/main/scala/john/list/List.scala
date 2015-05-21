package john.list

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  
  def isEmpty[A](x: List[A]): Boolean = x match {
    case Nil => true
    case _ => false
  }
  
  def head[A](l: List[A]): Option[A] = l match {
    case Nil => None
    case Cons(h, t) => Some(h) 
  }
  
  def tail[A](l: List[A]): Option[A] = l match {
    case Nil => None
    case Cons(h, Nil) => Some(h)
    case Cons(h, t) => tail(t)
  }
  
  def update[A](l: List[A], index: Int, y: A): List[A] = l match {
    case Nil => if(index == 0) Cons(y, Nil) else Nil
    case Cons(h, t) => if(index == 0) Cons(y,t) else update(t, index-1, y)
  }
  
  def ++[A](xs: List[A], ys: List[A]): List[A] = xs match {
    case Nil => ys
    case Cons(head, tail) => Cons(head, ++(tail, ys))
  }
  
  def suffixes[A](x: List[A]): List[List[A]] = x match {
    case Nil => Nil
    case Cons(head, tail) => Cons(x, suffixes(tail))
  }
  
  def apply[A](as: A*): List[A] = 
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}