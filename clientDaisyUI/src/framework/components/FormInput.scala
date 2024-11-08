package framework.components

import alleycats.Empty
import com.raquo.airstream.core.Signal
import com.raquo.laminar.api.L
import com.raquo.laminar.api.L.*
import com.raquo.laminar.inserters.DynamicInserter
import com.raquo.laminar.modifiers.Modifier
import com.raquo.laminar.nodes.{ChildNode, ReactiveElement, ReactiveHtmlElement}
import framework.data.FrameworkDate
import framework.sourcecode.DefinedAt
import framework.utils.{UpdatableSignal, Validatable}
import io.scalaland.chimney.Transformer
import org.scalajs.dom.{
  html,
  window,
  Comment,
  Element,
  HTMLDivElement,
  HTMLInputElement,
  HTMLLabelElement,
  HTMLTextAreaElement,
}

import scala.annotation.targetName
import framework.utils.ZoomedOwnerlessSignal
import framework.data.FormFileHolder
import monocle.Focus.MkFocus
import framework.localization.LocalizationSupport
import cats.syntax.all.*
import scala.util.chaining.*

/** Various helpers for form inputs. */
object FormInput {
  def idForLabel(label: String): String = {
    label.toLowerCase.replaceAll("\\W", "_")
  }

  def validationMessages[A](
    signal: Signal[A],
    validation: Option[Validatable[A]],
  ): Modifier[ReactiveElement[Element]] = {
    validation match {
      case None             => emptyNode
      case Some(validation) => validationMessages(signal)(using validation)
    }
  }

  def validationMessages[A](signal: Signal[A])(using validation: Validatable[A]): DynamicInserter = {
    val errorMsgs = signal.map(validation.validate(_).map { errors =>
      val classNameMod = cls := "text-error min-h-4"
      if (errors.errors.sizeIs == 1) p(classNameMod, errors.errors.head.message.asString)
      else ul(classNameMod, errors.errors.iterator.map { err => li(err.message.asString) }.toArray)
    })
    child.maybe <-- errorMsgs
  }

  def string[A](
    signal: ZoomedOwnerlessSignal[A],
    modifiers: Modifier[Input]*
  )(using asString: Transformer[A, String], asA: Transformer[String, A]): ReactiveHtmlElement[HTMLInputElement] = {
    input(
      `type` := "text",
      cls := "grow",
      controlled(
        value <-- signal.signal.map(asString.transform),
        onInput.mapToValue.map(asA.transform) --> signal.setTo,
      ),
      modifiers,
    )
  }

  def stringWithLabel[A](
    label: String,
    signal: ZoomedOwnerlessSignal[A],
    validation: Option[Validatable[A]],
    altLabel: Seq[Modifier[Span]] = Seq.empty,
    beforeLabel: Seq[Modifier[Div]] = Seq.empty,
    beforeInput: Seq[Modifier[Div]] = Seq.empty,
    afterInput: Seq[Modifier[Div]] = Seq.empty,
    inputModifiers: Seq[Modifier[Input]] = Seq.empty,
    withMargin: Boolean = true,
    placeholder: Option[String] = None,
  )(using Transformer[A, String], Transformer[String, A]): Div = {
    div(
      when(withMargin)(cls := "mb-4"),
      L.label(
        when(label.nonEmpty || altLabel.nonEmpty)(
          div(
            cls := "label",
            beforeLabel,
            when(label.nonEmpty)(span(cls := "label-text", label)),
            when(altLabel.nonEmpty)(span(cls := "label-text-alt", altLabel)),
          )
        ),
        div(
          cls := "input input-bordered flex items-center gap-2",
          beforeInput,
          string(signal, placeholder.map(L.placeholder := _), inputModifiers),
          afterInput,
        ),
      ),
      validationMessages(signal.signal, validation),
    )
  }

  def stringWithLabelLocalized[A](
    signal: ZoomedOwnerlessSignal[A],
    validation: Option[Validatable[A]],
    altLabel: Seq[Modifier[Span]] = Seq.empty,
    beforeLabel: Seq[Modifier[Div]] = Seq.empty,
    beforeInput: Seq[Modifier[Div]] = Seq.empty,
    afterInput: Seq[Modifier[Div]] = Seq.empty,
    inputModifiers: Seq[Modifier[Input]] = Seq.empty,
  )(using l18n: LocalizationSupport)(using Transformer[A, String], Transformer[String, A], l18n.LocaleEnum)(using
    lto: l18n.LocalizedTextOf[A]
  ): ReactiveHtmlElement[HTMLDivElement] =
    stringWithLabel(
      label = lto.text,
      signal = signal,
      validation = validation,
      altLabel = altLabel,
      beforeLabel = beforeLabel,
      beforeInput = beforeInput,
      afterInput = afterInput,
      inputModifiers = inputModifiers,
    )

  def stringWithLabelReadOnly[A](
    label: String,
    signal: Signal[A],
    beforeInput: Seq[Modifier[ReactiveHtmlElement[html.Element]]] = Seq.empty,
    afterInput: Seq[Modifier[ReactiveHtmlElement[html.Element]]] = Seq.empty,
    inputModifiers: Seq[Modifier[ReactiveHtmlElement[HTMLDivElement]]] = Seq.empty,
  )(using transformer: Transformer[A, String]): ReactiveHtmlElement[HTMLDivElement] = {
    div(
      cls := "mb-4",
      L.label(
        div(cls := "label", span(cls := "label-text", label)),
        div(
          cls := "input input-bordered flex items-center gap-2",
          beforeInput,
          div(
            inputModifiers,
            child.text <-- signal.map(transformer.transform),
          ),
          afterInput,
        ),
      ),
    )
  }

  def textArea[A](
    signal: ZoomedOwnerlessSignal[A],
    modifiers: Modifier[ReactiveHtmlElement[HTMLTextAreaElement]]*
  )(using asString: Transformer[A, String], asA: Transformer[String, A]): ReactiveHtmlElement[HTMLTextAreaElement] = {
    L.textArea(
      controlled(
        value <-- signal.signal.map(asString.transform),
        onInput.mapToValue.map(asA.transform) --> signal.setTo,
      ),
      modifiers,
    )
  }

  def textAreaWithLabel[A](
    label: String,
    signal: ZoomedOwnerlessSignal[A],
    validation: Option[Validatable[A]],
    beforeLabel: Seq[Modifier[Div]] = Seq.empty,
    altLabel: Seq[Modifier[Span]] = Seq.empty,
    textAreaModifiers: Seq[Modifier[ReactiveHtmlElement[HTMLTextAreaElement]]] = Seq.empty,
  )(using Transformer[A, String], Transformer[String, A]): ReactiveHtmlElement[HTMLLabelElement] = {
    val id = idForLabel(label)

    L.label(
      className := "form-control mb-4",
      forId := id,
      div(
        cls := "label",
        beforeLabel,
        span(cls := "label-text grow", label),
        when(altLabel.nonEmpty)(span(cls := "label-text-alt", altLabel)),
      ),
      textArea(signal, idAttr := id, cls := "textarea textarea-bordered h-24", textAreaModifiers),
      validationMessages(signal.signal, validation),
    )
  }

  def textAreaWithLabelLocalized[A](
    signal: ZoomedOwnerlessSignal[A],
    validation: Option[Validatable[A]],
    beforeLabel: Seq[Modifier[Div]] = Seq.empty,
    altLabel: Seq[Modifier[Span]] = Seq.empty,
    textAreaModifiers: Seq[Modifier[ReactiveHtmlElement[HTMLTextAreaElement]]] = Seq.empty,
  )(using l18n: LocalizationSupport)(using Transformer[A, String], Transformer[String, A], l18n.LocaleEnum)(using
    lto: l18n.LocalizedTextOf[A]
  ): ReactiveHtmlElement[HTMLLabelElement] =
    textAreaWithLabel(
      label = lto.text,
      signal = signal,
      validation = validation,
      beforeLabel = beforeLabel,
      altLabel = altLabel,
      textAreaModifiers = textAreaModifiers,
    )

  def textAreaWithLabelReadOnly[A](
    label: String,
    signal: Signal[A],
    beforeLabel: Seq[Modifier[ReactiveHtmlElement[html.Element]]] = Seq.empty,
    textAreaModifiers: Seq[Modifier[ReactiveHtmlElement[HTMLDivElement]]] = Seq.empty,
  )(using t: Transformer[A, String]): ReactiveHtmlElement[HTMLLabelElement] = {
    val id = idForLabel(label)

    L.label(
      className := "form-control mb-4",
      div(
        cls := "label",
        beforeLabel,
        span(cls := "label-text grow", label),
      ),
      div(
        idAttr := id,
        cls := "textarea textarea-bordered h-24",
        textAreaModifiers,
        children <-- signal.map { a =>
          val str = t.transform(a)
          str.linesIterator.map(p(_)).toVector
        },
      ),
    )
  }

  def textLikeWithLabelLocalized[A](
    signal: ZoomedOwnerlessSignal[A],
    validation: Option[Validatable[A]],
    beforeLabel: Seq[Modifier[ReactiveHtmlElement[html.Element]]] = Seq.empty,
    altLabel: Seq[Modifier[Span]] = Seq.empty,
    inputModifiers: Seq[HtmlMod] = Seq.empty,
  )(using l18n: LocalizationSupport)(using Transformer[A, String], Transformer[String, A], l18n.LocaleEnum)(using
    lto: l18n.LocalizedTextOf[A],
    textKind: TextKindFor[A],
  ) = textKind.textKind match {
    case TextKind.SingleLine =>
      stringWithLabelLocalized(
        signal = signal,
        validation = validation,
        beforeLabel = beforeLabel,
        altLabel = altLabel,
        inputModifiers = inputModifiers,
      )

    case TextKind.MultiLine =>
      textAreaWithLabelLocalized(
        signal = signal,
        validation = validation,
        beforeLabel = beforeLabel,
        altLabel = altLabel,
        textAreaModifiers = inputModifiers,
      )
  }

  def date[A](
    signal: ZoomedOwnerlessSignal[A],
    validation: Option[Validatable[A]],
    modInput: L.Input => L.Modifier[L.Label] = input => input,
  )(using
    toDate: Transformer[A, FrameworkDate],
    fromDate: Transformer[FrameworkDate, A],
  ): ReactiveHtmlElement[HTMLLabelElement] = {
    L.label(
      className := "form-control mb-4",
      modInput(
        L.input(
          cls := "input input-bordered w-40 pl-3 pr-0",
          `type` := "date",
          controlled(
            value <-- signal.signal.map(toDate.transform(_).asString),
            onInput.mapToValue.map(str => fromDate.transform(FrameworkDate.fromString(str).getOrThrow)) --> signal.setTo,
          ),
        )
      ),
      validationMessages(signal.signal, validation),
    )
  }

  def dateWithLabel[A](
    label: String,
    signal: ZoomedOwnerlessSignal[A],
    validation: Option[Validatable[A]],
    beforeLabel: Seq[Modifier[Div]] = Seq.empty,
    altLabel: Seq[Modifier[Span]] = Seq.empty,
    modInput: L.Input => L.Modifier[L.Element] = input => input,
    renderPosition: RenderPosition = RenderPosition.Below,
  )(using
    toDate: Transformer[A, FrameworkDate],
    fromDate: Transformer[FrameworkDate, A],
  ): ReactiveHtmlElement[HTMLLabelElement] = {
    date(
      signal,
      validation,
      modInput = inputElem => {
        val labelElem = Option.when(label.nonEmpty)(
          span(
            cls := "label-text grow",
            renderPosition match {
              case RenderPosition.Below    => label
              case RenderPosition.Sideways => show"$label:"
            },
          )
        )

        val altLabelElem = Option.when(altLabel.nonEmpty)(
          span(
            cls := "label-text-alt",
            altLabel,
          )
        )

        val newInputElem = modInput(inputElem)

        renderPosition match {
          case RenderPosition.Below =>
            div(
              when(labelElem.isDefined || altLabelElem.isDefined)(
                div(cls := "label", beforeLabel, labelElem, altLabelElem)
              ),
              newInputElem,
            )
          case RenderPosition.Sideways =>
            div(
              cls := "flex grid-cols-2 gap-4 justify-start content-start",
              labelElem.map(elem => div(cls := "label", elem)),
              newInputElem,
              altLabelElem.map(elem => nodeSeq(div(cls := "grow"), div(cls := "label", elem))),
            )
        }
      },
    )
  }

  def dateWithLabelLocalized[A](
    signal: ZoomedOwnerlessSignal[A],
    validation: Option[Validatable[A]],
    beforeLabel: Seq[Modifier[Div]] = Seq.empty,
    altLabel: Seq[Modifier[Span]] = Seq.empty,
    modInput: L.Input => L.Modifier[L.Element] = input => input,
    renderPosition: RenderPosition = RenderPosition.Below,
  )(using
    l18n: LocalizationSupport
  )(using Transformer[A, FrameworkDate], Transformer[FrameworkDate, A], l18n.LocaleEnum)(using
    lto: l18n.LocalizedTextOf[A]
  ): ReactiveHtmlElement[HTMLLabelElement] =
    dateWithLabel(
      label = lto.text,
      signal = signal,
      validation = validation,
      beforeLabel = beforeLabel,
      altLabel = altLabel,
      modInput = modInput,
      renderPosition = renderPosition,
    )

  /** A select which always has an option selected. */
  def select[A](
    selected: ZoomedOwnerlessSignal[A],
    options: NonEmptyVector[(A, String)],
    beforeSelect: Seq[L.Node] = Seq.empty,
    beforeChange: () => Boolean = () => true,
    style: ComponentStyle = ComponentStyle.Standalone,
  )(using CanEqual[A, A], DefinedAt) = {
    val selectElem = selectElement(
      selected,
      options,
      beforeChange = beforeChange,
    ).amend(
      cls := (style match {
        case ComponentStyle.Standalone => "w-full"
        case ComponentStyle.TableCell  => "select-xs"
      })
    )

    style match {
      case ComponentStyle.Standalone =>
        nodeSeq(
          label(
            cls := "form-control w-full max-w-xs mb-4 input input-bordered",
            div(cls := "flex items-center", beforeSelect, selectElem),
          )
        )
      case ComponentStyle.TableCell =>
        beforeSelect ++ nodeSeq(selectElem)
    }
  }

  /** A select which always has an option selected. */
  def selectElement[A: CanEqual1](
    selected: ZoomedOwnerlessSignal[A],
    options: NonEmptyVector[(A, String)],
    beforeChange: () => Boolean = () => true,
  )(using DefinedAt) = {
    L.select(
      cls := "select select-ghost pl-3 pr-0 min-w-14 max-w-xs",
      controlled(
        value <-- selected.signal.map { value =>
          val name = options.find(_._1 == value) match {
            case None =>
              logError(s"Value $value not found in $options, this should never happen and is a bug.")
              "it's a bug :("
            case Some((_, name)) => name
          }

          name
        },
        onChange.filter(_ => beforeChange()).mapToValue.map { name =>
          options
            .find(_._2 == name)
            .getOrElse {
              logError(s"Option $name not found in $options, this should never happen and is a bug.")
              options.head
            }
            ._1
        } --> selected.setTo,
      ),
      options.toVector.map { case (_, name) =>
        option(value := name, name)
      },
    )
  }

  def selectWithLabel[A](
    title: String,
    selected: ZoomedOwnerlessSignal[Option[A]],
    options: Vector[(A, String)],
    beforeSelect: Seq[Modifier[ReactiveHtmlElement[html.Element]]] = Seq.empty,
    beforeChange: () => Boolean = () => true,
  )(using CanEqual[A, A]) = {
    label(
      cls := "form-control w-full max-w-xs mb-4 input input-bordered",
      div(
        cls := "flex items-center",
        beforeSelect,
        L.select(
          cls := "select select-ghost w-full max-w-xs pl-3 pr-0",
          controlled(
            value <-- selected.signal.map {
              case None        => title
              case Some(value) => options.find(_._1 == value).fold(title)(_._2)
            },
            onChange.filter(_ => beforeChange()).mapToValue.map { name =>
              options.find(_._2 == name).map(_._1)
            } --> selected.setTo,
          ),
          option(disabled := true, L.selected := true, value := title, title),
          options.map { case (_, name) =>
            option(value := name, name)
          },
        ),
      ),
    )
  }

  def bigDecimal[A](
    signal: ZoomedOwnerlessSignal[A],
    modifiers: Modifier[Input]*
  )(using fromA: Transformer[A, BigDecimal], toA: Transformer[BigDecimal, A]): ReactiveHtmlElement[HTMLInputElement] = {
    input(
      `type` := "number",
      modifiers,
      controlled(
        value <-- signal.signal.map(fromA.transform(_).show),
        onInput.mapToValueBigDecimal.orElseEmpty.map(toA.transform) --> signal.setTo,
      ),
    )
  }

  @targetName("bigDecimalOption")
  def bigDecimal[A](
    signal: ZoomedOwnerlessSignal[Option[A]],
    modifiers: Modifier[ReactiveHtmlElement[HTMLInputElement]]*
  )(using fromA: Transformer[A, BigDecimal], toA: Transformer[BigDecimal, A]): ReactiveHtmlElement[HTMLInputElement] = {
    input(
      `type` := "number",
      modifiers,
      controlled(
        value <-- signal.signal.map(_.map(fromA.transform(_).show).getOrElse("")),
        onInput.mapToValueBigDecimal.map(_.map(toA.transform)) --> signal.setTo,
      ),
    )
  }

  def file(
    holder: UpdatableSignal[FormFileHolder],
    inputModifiers: Seq[Modifier[Input]] = Seq.empty,
  ) = {
    div(
      input(
        `type` := "file",
        cls := "file-input file-input-bordered file-input-md w-full max-w-xs",
        accept <-- holder.signal.map(_.acceptAttributeValue),
        onChange.mapToFiles.map(_.headOption) --> { file => holder.update(_.withFile(file)) },
        inputModifiers,
      ),
      validationMessages(holder.signal, Some(summon)),
    )
  }

  def fileWithLabel(
    label: String,
    holder: UpdatableSignal[FormFileHolder],
    altLabel: String = "",
    inputModifiers: Seq[Modifier[Input]] = Seq.empty,
  ) = {
    div(
      cls := "mb-4",
      div(
        cls := "label",
        span(cls := "label-text", label),
        when(altLabel.nonEmpty)(span(cls := "label-text-alt", altLabel)),
      ),
      file(holder, inputModifiers),
    )
  }

  /** Renders a field from the form if it is [[Some]], otherwise renders an empty node.
    *
    * Example:
    * {{{
    * child <-- FormInput.optional(
    *   form.signal.bimap(_.shipper)(_.focus(_.shipper).replace(_))
    * ) { signal =>
    *   FormInput.stringWithLabel(
    *     "Shipper",
    *     signal,
    *     validation = Some(summon),
    *     inputModifiers = Vector(disabled <-- form.submitting),
    *   )
    * }
    * }}}
    */
  def optional[Field](
    fieldSignal: UpdatableSignal[Option[Field]]
  )(render: UpdatableSignal[Option[Field]] ?=> UpdatableSignal[Field] => L.Element): Signal[L.Node] = {
    fieldSignal.signal
      .splitOption(
        (_, signal) => {
          val updatableFieldSignal = UpdatableSignal(
            signal,
            () =>
              fieldSignal
                .now()
                .getOrElse(
                  throw new IllegalStateException(
                    "This branch should only be executed when the `Option` is `Some`, thus `get` should be safe! " +
                      "This is a bug."
                  )
                ),
            value => fieldSignal.setTo(Some(value)),
          )

          render(using fieldSignal)(updatableFieldSignal)
        },
        emptyNode,
      )
  }

  def checkbox[Field](
    label: String,
    fieldSignal: UpdatableSignal[Field],
    inputModifiers: Seq[Modifier[Input]] = Seq.empty,
    filterEvent: Boolean => Boolean = _ => true,
  )(using toBool: Transformer[Field, Boolean], fromBool: Transformer[Boolean, Field]): Label = {
    L.label(
      cls := "label",
      input(
        `type` := "checkbox",
        cls := "checkbox",
        inputModifiers,
        controlled(
          checked <-- fieldSignal.signal.map(toBool.transform),
          onClick.mapToChecked.filter(filterEvent) --> { value => fieldSignal.setTo(fromBool.transform(value)) },
        ),
      ),
      span(cls := "label-text ml-2", label),
    )
  }

  /** Renders an optional field that has a checkbox to enable it. */
  def optionalWithCheckbox[Field](
    fieldSignal: UpdatableSignal[Option[Field]],
    renderPosition: RenderPosition = RenderPosition.Below,
  )(render: UpdatableSignal[Option[Field]] ?=> UpdatableSignal[Field] => L.Element)(using
    l18n: LocalizationSupport,
    lto: l18n.LocalizedTextOf[Field],
    locale: l18n.LocaleEnum,
    empty: Empty[Field],
  ): L.Element = {
    val isEnabledSignal = fieldSignal.signal.map(_.isDefined)

    div(
      cls := "form-control",
      // STYLE: make the label take as little space as possible
      when(renderPosition.isSideways)(cls := "grid grid-cols-2 gap-4 justify-start content-start"),
      label(
        cls := "label cursor-pointer justify-start content-start",
        when(renderPosition.isSideways)(cls := "shrink"),
        input(
          `type` := "checkbox",
          cls := "checkbox mr-2",
          cls := (renderPosition match {
            case RenderPosition.Below    => "checkbox-sm"
            case RenderPosition.Sideways => "checkbox-lg"
          }),
          controlled(
            checked <-- isEnabledSignal,
            onClick.mapToChecked --> { checked =>
              fieldSignal.setTo(if (checked) Some(empty.empty) else None)
            },
          ),
        ),
        span(cls := "label-text", lto.text),
      ),
      child <-- optional(fieldSignal)(render(using fieldSignal)),
    )
  }

  /** As [[optional]] but returns the field name as well. */
  def optionalWithFieldName[Field](
    fieldSignal: UpdatableSignal[Option[Field]]
  )(
    render: UpdatableSignal[Option[Field]] ?=> UpdatableSignal[Field] => L.Element
  )(using
    l18n: LocalizationSupport
  )(using lto: l18n.LocalizedTextOf[Field], locale: l18n.LocaleEnum): (Signal[L.Node], String) = {
    val fieldName = lto.text
    val signal = optional(fieldSignal)(render)

    (signal, fieldName)
  }

  def oneOf[Field, Kind: CanEqual1](
    fieldSignal: UpdatableSignal[Field],
    kindLens: MkFocus[Field] => Lens[Field, Kind],
    kindOptions: NonEmptyVector[(Kind, String)],
  ) = {
    val kindRx = fieldSignal.bimapGenLens(kindLens)
    FormInput.select(kindRx, kindOptions)
  }

  /** A field description for [[extraFieldsSelector]].
    *
    * @param name
    *   the name of the field shown in the selector
    * @param isShown
    *   whether the field is currently shown to the user
    * @param hide
    *   hides the field
    * @param show
    *   shows the field
    */
  case class ExtraField[Form](name: String, isShown: Form => Boolean, hide: Form => Form, show: Form => Form) {
    val lowercaseName: String = name.toLowerCase()

    def toggle(form: Form): Form = if (isShown(form)) hide(form) else show(form)
  }
  object ExtraField {

    /** Creates an [[ExtraField]] for a field wrapped in [[Option]]. */
    def optional[Form, A: Empty](
      name: String,
      lens: Lens[Form, Option[A]],
    ): ExtraField[Form] =
      apply(
        name,
        isShown = form => lens.get(form).isDefined,
        hide = form => lens.replace(None)(form),
        show = form => lens.replace(empty[A].some)(form),
      )

    /** Creates an [[ExtraField]] for a field wrapped in [[Option]], gets the name from [[LocalizedTextOf]]. */
    def optional[Form, A: Empty](
      lens: Lens[Form, Option[A]]
    )(using
      l18n: LocalizationSupport
    )(using lto: l18n.LocalizedTextOf[A], locale: l18n.LocaleEnum): ExtraField[Form] =
      optional(lto.text, lens)

    /** Creates an [[ExtraField]] for a field wrapped in [[Option]], gets the name from [[LocalizedTextOf]]. */
    def optional[Form, A: Empty](
      makeLens: MkFocus[Form] => Lens[Form, Option[A]]
    )(using
      l18n: LocalizationSupport
    )(using lto: l18n.LocalizedTextOf[A], locale: l18n.LocaleEnum): ExtraField[Form] =
      optional(lto.text, makeLens(GenLens[Form]))
  }

  /** Content for [[extraFieldsSelector]]. */
  trait ExtraFieldsSelectorContent {
    def addExtraFieldsText: String

    def addExtraFieldsText(currentFieldCount: Int): String

    def removeIcon: L.Node
    def addIcon: L.Node
    def searchIcon: L.Node
  }

  /** Draws a selector for selecting extra fields in the form.
    *
    * @param fromTop
    *   whether the dropdown should open from the top
    * @param enableSorting
    *   whether the fields should be sorted alphabetically
    */
  def extraFieldsSelector[Form](
    formSignal: UpdatableSignal[Form],
    fromTop: Boolean = true,
    enableSorting: Boolean = true,
  )(
    fields: NonEmptyVector[ExtraField[Form]]
  )(using content: ExtraFieldsSelectorContent) = {
    val searchBar = Var("")
    val focusSearchBus = EventBus[Unit]()
    val processedSearchTerm = searchBar.signal.map(_.trim.toLowerCase())
    val selectedFieldCount = formSignal.signal.map(form => fields.iterator.count(_.isShown(form)))

    div(
      cls := "dropdown",
      when(fromTop)(cls := "dropdown-top"),
      div(
        tabIndex := 0,
        role := "button",
        cls := "btn mb-4",
        onClick.mapToUnit --> focusSearchBus.writer,
        child.text <-- selectedFieldCount.map {
          case 0 => content.addExtraFieldsText
          case n => content.addExtraFieldsText(n)
        },
      ),
      div(
        cls := "dropdown-content shadow menu p-2 z-[1] bg-base-200 rounded-box w-96 gap-2",
        ul(
          cls := "overflow-y-auto max-h-64",
          fields.toVector.pipe(fields => if (enableSorting) fields.sortBy(_.name) else fields).map { field =>
            val fieldEnabled = formSignal.signal.map(field.isShown)
            val shownBySearch = processedSearchTerm.map {
              case ""     => true
              case needle => field.lowercaseName.contains(needle)
            }

            child <-- shownBySearch.splitBoolean(
              whenTrue = _ =>
                li(
                  button(
                    `type` := "button",
                    cls := "btn btn-sm justify-start",
                    cls <-- fieldEnabled.map(if (_) Vector("btn-active") else Vector.empty),
                    child <-- fieldEnabled.map(if (_) content.removeIcon else content.addIcon),
                    field.name,
                    onClick --> { _ => formSignal.update(field.toggle) },
                  )
                ),
              whenFalse = _ => emptyNode,
            )
          },
        ),
        when(fields.size > 7) {
          stringWithLabel(
            "Search",
            searchBar,
            validation = None,
            beforeInput = Vector(content.searchIcon),
            inputModifiers = Vector(focus <-- focusSearchBus.events.mapToStrict(true)),
            withMargin = false,
          ).amend(cls := "mt-2")
        },
      ),
    )
  }

  /** Content for [[removeOptionalFieldButton]]. */
  trait RemoveOptionalFieldButtonContent {
    def beforeLabel: Option[L.Node]

    def removeFieldText: Signal[String]

    /** "Are you sure you want to remove this field?" text with the localized name of the field. */
    def removalConfirmationText(fieldName: LocalizationSupport.AppliedLocalizedText): Signal[String]
  }

  /** Renders a button that removes an optional field.
    *
    * You most likely want to create a single [[RemoveOptionalFieldButtonContent]] instance for all your buttons in your
    * application.
    *
    * Example:
    * {{{
    * form.signal.bimapGenLens(_(_.originOfGoods)).pipe { optSignal =>
    *   FormInput.optionalWithFieldName(optSignal)(signal =>
    *     FormInput.stringWithLabelLocalized(
    *       signal,
    *       validation = PerformValidations.summon,
    *       inputModifiers = Vector(disabled <-- form.submitting),
    *       altLabel = Seq(FormInput.removeOptionalFieldButton(optSignal)),
    *     )
    *   )
    * }
    * }}}
    */
  def removeOptionalFieldButton[A: Empty: CanEqual1](
    signal: UpdatableSignal[Option[A]]
  )(using
    l18n: LocalizationSupport
  )(using l18n.LocaleEnum)(using lto: l18n.LocalizedTextOf[A], content: RemoveOptionalFieldButtonContent) = {
    div(
      cls := "btn btn-sm btn-secondary btn-outline",
      content.beforeLabel,
      child.text <-- content.removeFieldText,
      onClick(_.sample(content.removalConfirmationText(lto.applied))) --> { removalConfirmationText =>
        val current = signal.now()
        val needsConfirmation = current.exists(_.isNonEmpty)

        if (!needsConfirmation || window.confirm(removalConfirmationText)) {
          signal.setTo(None)
        }
      },
    )
  }
}
