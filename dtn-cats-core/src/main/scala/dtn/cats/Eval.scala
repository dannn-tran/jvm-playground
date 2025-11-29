package dtn.cats

sealed trait Eval[T]:
  def value: T
  def flatMap(f: T => Eval[T]): Eval[T] = Eval.defer(f(value))

object Eval:
  def now[T](x: T): Eval[T] = new Eval:
    override def value: T = x

  def later[T](x: => T): Eval[T] = new Eval:
    private var memoized: Option[T] = None
    override def value: T = memoized match {
      case Some(value) => value
      case None =>
        val v = x
        memoized = Some(v)
        v
    }

  def always[T](x: => T): Eval[T] = new Eval:
    override def value: T = x

  def defer[T](x: => Eval[T]): Eval[T] = new Eval[T]:
    private var memoized: Option[Eval[T]] = None
    override def value: T = memoized match {
      case Some(value) => value.value
      case None =>
        val v = x
        memoized = Some(v)
        v.value
    }

