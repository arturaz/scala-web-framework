package framework.exts

extension (stream: EventStream[Boolean]) {
  def collectTrues: EventStream[Unit] = stream.collect { case true => () }

  def collectFalses: EventStream[Unit] = stream.collect { case false => () }
}
