package framework.utils.assembler

import framework.exts.*

/** Converts relational data to hierarchical data. */
object Assembler {

  /** Processes results that you get after an SQL join.
    *
    * If you have two tables, DrugRequests and PharmacyResponses and execute a query like:
    * ```
    * SELECT reqs.*, responses.* FROM DrugRequests reqs
    * LEFT JOIN PharmacyResponses responses ON reqs.id = responses.requestId
    * ```
    *
    * Then you can use `Assembler` to convert the results to a hierarchical structure:
    * ```scala
    * val results: Vector[(OutstandingDrugRequestDB, Option[ListDrugRequestsPharmacyResponse])]
    *
    * val itemsForGP: Vector[ListDrugRequestsItem] =
    *   Assembler
    *     .fromJoinResult(
    *       items = results,
    *       itemToId = { case (req, _) => req.id },
    *       itemToChild = { case (_, response) => response },
    *       itemToResult = { case ((req, _), responses) =>
    *         ListDrugRequestsItem(req.id, req.data, responses.toVector, req.acceptedResponse)
    *       },
    *     )
    *     .toVector
    * ```
    *
    * @param items
    *   The joined rows.
    * @param itemToId
    *   Extracts the ID of an item from the joined row.
    * @param itemToChild
    *   Extracts the child (which can be non-existent, thus [[Option]]) of an item from the joined row.
    * @param itemToResult
    *   Converts an item and its present children to a result.
    * @return
    *   An iterator of results.
    */
  def fromJoinResult[Item, ItemId, Child, Result](
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

  /** Processes results from two separate queries.
    *
    * For example, you have two tables, DrugRequests and PharmacyResponses, and execute SQL queries:
    * ```sql
    * SELECT * FROM DrugRequests reqs
    * ```
    *
    * and
    *
    * ```sql
    * SELECT * FROM PharmacyResponses responses
    * ```
    *
    * Then you can use `Assembler` to convert the results to a hierarchical structure:
    * ```scala
    * val requests: Vector[DrugRequests.Row]
    * val responses: Vector[PharmacyResponses.Row]
    *
    * val itemsForGP: Vector[ListDrugRequestsItem] =
    *   Assembler
    *     .fromSeparateQueries(
    *       parentRows = requests,
    *       childRows = responses,
    *       parentToId = { req => req.id },
    *       childToParentId = { response => response.requestId },
    *       childToResult = { (req, responses) =>
    *         ListDrugRequestsItem(req.id, req.data, responses.toVector, req.acceptedResponse)
    *       },
    *     )
    *     .toVector
    * ```
    *
    * @param parentRows
    *   The rows of the parent table.
    * @param parentToId
    *   Extracts the ID of a parent from a row.
    * @param childRows
    *   The rows of the children table.
    * @param childToParentId
    *   Extracts the ID of a child from a row.
    * @param childToResult
    *   Converts a parent and its children to a result.
    * @return
    *   An iterator of results.
    */
  def fromSeparateQueries[ParentRow, ParentId, ChildRow, Result](
    parentRows: Iterable[ParentRow],
    parentToId: ParentRow => ParentId,
    childRows: Iterable[ChildRow],
    childToParentId: ChildRow => ParentId,
    childToResult: (ParentRow, Iterator[ChildRow]) => Result,
  ): Iterator[Result] = {
    val childrenByParentId = childRows.groupBy(childToParentId)

    parentRows.iterator.map { parentRow =>
      val parentId = parentToId(parentRow)
      val children = childrenByParentId.get(parentId).fold2(Iterator.empty, _.iterator)
      childToResult(parentRow, children)
    }
  }
}
