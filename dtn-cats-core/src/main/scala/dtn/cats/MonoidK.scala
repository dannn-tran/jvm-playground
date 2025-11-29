package dtn.cats

trait MonoidK[F[_]]:
  def empty[T]: F[T]
  def combineK[T](fa: F[T], fb: F[T]): F[T]
  
object MonoidK:
  def apply[F[_]](using m: MonoidK[F]): MonoidK[F] = m
  
  val listMonoidK: MonoidK[List] = new MonoidK[List]:
    override def empty[T]: List[T] = List.empty
    override def combineK[T](fa: List[T], fb: List[T]): List[T] = List.concat(fa, fb)

  def optMonoidK: MonoidK[Option] = new MonoidK[Option]:
    override def empty[T]: Option[T] = None
    override def combineK[T](fa: Option[T], fb: Option[T]): Option[T] = fb.orElse(fa)
    