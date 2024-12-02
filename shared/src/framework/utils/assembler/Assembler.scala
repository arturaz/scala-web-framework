package framework.utils.assembler

/** Converts relational data to hierarchical data. */
object Assembler {

  /** Example:
    * {{{
    * val children: Vector[(OutstandingDrugRequestDB, Option[ListDrugRequestsPharmacyResponse])] = ???
    *
    * val itemsForGP: Vector[ListDrugRequestsItem] =
    *   Assembler(
    *     children,
    *     { case (req, _) => req.id },
    *     { case (_, response) => response },
    *     { case ((req, _), responses) =>
    *       ListDrugRequestsItem(req.id, req.data, responses.toVector, req.acceptedResponse)
    *     },
    *   )
    *   .toVector
    * }}}
    *
    * @return
    */
  def apply[Item, ItemId, Child, Result](
    items: Iterable[Item],
    itemToId: Item => ItemId,
    itemToChild: Item => Option[Child],
    itemToResult: (Item, Iterator[Child]) => Result,
  ): Iterator[Result] = {
    val itemById = items.groupBy(itemToId)
    val results = itemById.iterator
      .map { case (itemId, items) =>
        // Safe because `groupBy` ensures that `items` is non-empty
        val item = items.head
        val children = items.iterator.flatMap(itemToChild)
        itemToResult(item, children)
      }

    results
  }
}
