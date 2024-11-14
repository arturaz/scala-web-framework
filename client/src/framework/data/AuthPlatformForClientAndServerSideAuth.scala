package framework.data

import framework.components.PageLoadingIndicator
import framework.sourcecode.DefinedAt
import framework.utils.PageRenderResult

/** Provides an authentication platform for authentication which works in this way:
  *
  *   - The client uses a client-side library (for example Firebase, Supabase, Auth0, etc.) to obtain an authentication
  *     token along with any other data.
  *   - The token is sent to the application server for validation, retrieving additional data.
  */
trait AuthPlatformForClientAndServerSideAuth {

  /** The type of a page in the application. */
  type TPage

  /** The default page of the application. Users would open this if they just open your domain. */
  case class RootPage(page: TPage)

  /** The type of client-side authentication data. Some of it will be sent to server later. */
  type TClientSideAuthData

  /** The type of client-side data which is returned when authentication fails on the client side. */
  type TClientSideAuthFailedData

  /** The type of authentication data retrieved from the server. */
  type TServerSideAuthData

  /** The type of data server sends us when authentication fails. */
  type TServerSideAuthFailedData

  /** States where client-side authentication has already responded, either in success or failure. */
  sealed trait ClientAuthClientSideHasRespondedState

  /** States which have the client side authentication data, as in client-side auth has succeeded. */
  sealed trait ClientAuthClientSideHasSucceededState extends ClientAuthClientSideHasRespondedState {
    def clientData: TClientSideAuthData
  }

  /** States which have the client side authentication data and the server has responded. */
  sealed trait ClientAuthServerSideHasRespondedState extends ClientAuthClientSideHasSucceededState

  /** States where either client or server side authentication has failed. */
  sealed trait ClientAuthHasFailedState extends ClientAuthClientSideHasRespondedState

  /** Authentication state, stored on the client.
    *
    * @param maybeRedirectToAfterRegistration
    *   page to which the user should be redirected to after he/she authenticates
    */
  enum ClientAuthState(val maybeRedirectToAfterRegistration: Option[TPage]) derives CanEqual {

    /** We are determining whether the user is authenticated. */
    case Loading(override val maybeRedirectToAfterRegistration: Option[TPage])
        extends ClientAuthState(maybeRedirectToAfterRegistration)

    /** The user is not authenticated with the client-side authentication provider.
      *
      * @param failure
      *   if we are not authenticated due to client-side provider failure, this will be [[Some]]
      */
    case NotAuthenticated(
      failure: Option[TClientSideAuthFailedData],
      override val maybeRedirectToAfterRegistration: Option[TPage],
    ) extends ClientAuthState(maybeRedirectToAfterRegistration) with ClientAuthHasFailedState

    /** The user is authenticated in client-side, but not yet validated on the server side. */
    case AuthenticatedInClientSide(
      clientData: TClientSideAuthData,
      override val maybeRedirectToAfterRegistration: Option[TPage],
    ) extends ClientAuthState(maybeRedirectToAfterRegistration) with ClientAuthClientSideHasSucceededState

    /** Server has told us that the user is not known in the server side and has to register with the server side. */
    case AuthenticatedInClientSideButNotServerSide(
      clientData: TClientSideAuthData,
      serverData: TServerSideAuthFailedData,
      override val maybeRedirectToAfterRegistration: Option[TPage],
    ) extends ClientAuthState(maybeRedirectToAfterRegistration)
        with ClientAuthServerSideHasRespondedState
        with ClientAuthHasFailedState

    /** We are authenticated with both client and server sides. */
    case Authenticated(
      clientData: TClientSideAuthData,
      serverData: TServerSideAuthData,
    ) extends ClientAuthState(None) with ClientAuthServerSideHasRespondedState

    /** Whether we are currently performing an operation that is loading the state, either from client or server side
      * auth.
      */
    def isLoading: Boolean = this match {
      case _: Loading | _: AuthenticatedInClientSide                                             => true
      case _: NotAuthenticated | _: AuthenticatedInClientSideButNotServerSide | _: Authenticated => false
    }

    /** Returns [[Some]] if the client-side authentication provider has already responded. */
    def asClientSideHasRespondedState: Option[ClientAuthState & ClientAuthClientSideHasRespondedState] = this match {
      case _: Loading                                   => None
      case v: NotAuthenticated                          => Some(v)
      case v: AuthenticatedInClientSide                 => Some(v)
      case v: AuthenticatedInClientSideButNotServerSide => Some(v)
      case v: Authenticated                             => Some(v)
    }

    /** Returns [[Some]] if we have been authenticated in the client side. */
    def asClientSideHasSucceededState: Option[ClientAuthState & ClientAuthClientSideHasSucceededState] =
      this match {
        case _: Loading | _: NotAuthenticated             => None
        case v: AuthenticatedInClientSide                 => Some(v)
        case v: AuthenticatedInClientSideButNotServerSide => Some(v)
        case v: Authenticated                             => Some(v)
      }

    /** Returns [[Some]] if we have been authenticated in the client and server has responded. */
    def asServerSideHasRespondedState: Option[ClientAuthState & ClientAuthServerSideHasRespondedState] =
      this match {
        case _: Loading | _: NotAuthenticated | _: AuthenticatedInClientSide => None
        case v: AuthenticatedInClientSideButNotServerSide                    => Some(v)
        case v: Authenticated                                                => Some(v)
      }

    /** Returns [[Some]] if authentication has failed either on client or server side. */
    def asAuthHasFailedState: Option[ClientAuthState & ClientAuthHasFailedState] = this match {
      case _: Loading | _: Authenticated | _: AuthenticatedInClientSide => None
      case v: AuthenticatedInClientSideButNotServerSide                 => Some(v)
      case v: NotAuthenticated                                          => Some(v)
    }

    /** Whether we have fully finished authenticating. */
    def asAuthenticated: Option[Authenticated] = this match {
      case _: Loading | _: NotAuthenticated | _: AuthenticatedInClientSide |
          _: AuthenticatedInClientSideButNotServerSide =>
        None
      case v: Authenticated => Some(v)
    }

    /** Whether we have fully finished authenticating. */
    def isAuthenticated: Boolean = asAuthenticated.isDefined

    /** Stores the application page to which we should go after server confirms our authentication.
      *
      * @return
      *   [[None]] if we are already authenticated
      */
    def withRedirectToAfterAuthentication(page: TPage): Option[ClientAuthState] = this match {
      case v: Loading                                   => Some(v.copy(maybeRedirectToAfterRegistration = Some(page)))
      case v: NotAuthenticated                          => Some(v.copy(maybeRedirectToAfterRegistration = Some(page)))
      case v: AuthenticatedInClientSide                 => Some(v.copy(maybeRedirectToAfterRegistration = Some(page)))
      case v: AuthenticatedInClientSideButNotServerSide => Some(v.copy(maybeRedirectToAfterRegistration = Some(page)))
      case _: Authenticated                             => None
    }

    /** As [[withRedirectToAfterAuthentication]], but logs the result and always returns a new state. */
    def withRedirectToAfterAuthenticationWithLogging(
      page: TPage,
      log: Any => DefinedAt ?=> Unit = log,
      logError: Any => DefinedAt ?=> Unit = logError,
    )(using DefinedAt, CanEqual1[TPage]): ClientAuthState = {
      this match {
        case ClientAuthState.WithRedirectE(Some(`page`)) =>
          log(s"`withRedirectToAfterAuthenticationWithLogging`: ignoring, already set to $page")
          this
        case state =>
          state.withRedirectToAfterAuthentication(page) match {
            case None =>
              logError(s"`withRedirectToAfterAuthenticationWithLogging`: can't set as $page, current state is $state")
              this
            case Some(state) =>
              log(s"`withRedirectToAfterAuthenticationWithLogging`: set to $page, state is $state")
              state
          }
      }
    }

    /** The [[TPage]] to redirect to after we are registered. */
    def redirectToAfterRegistration(using root: RootPage): TPage =
      maybeRedirectToAfterRegistration.getOrElse(root.page)

    /** Becomes [[NotAuthenticated]], unless it already is, in which case the `failure` is replaced.. */
    def toNotAuthenticated(failure: Option[TClientSideAuthFailedData]): ClientAuthState =
      NotAuthenticated(failure, maybeRedirectToAfterRegistration)

    /** Becomes [[AuthenticatedInClientSide]], unless it already is, in which case the `clientData` is replaced.. */
    def toAuthenticatedInClientSide(clientData: TClientSideAuthData): ClientAuthState =
      AuthenticatedInClientSide(clientData, maybeRedirectToAfterRegistration)

    /** If it's a valid transition, returns [[Some]]([[AuthenticatedInClientSideButNotServerSide]])), otherwise
      * [[None]].
      */
    def withAuthenticationFailedOnServerSide(data: TServerSideAuthFailedData): Option[ClientAuthState] = this match {
      case _: Loading | _: NotAuthenticated => None
      case v: AuthenticatedInClientSide =>
        Some(AuthenticatedInClientSideButNotServerSide(v.clientData, data, v.maybeRedirectToAfterRegistration))
      case v: AuthenticatedInClientSideButNotServerSide => Some(v.copy(serverData = data))
      case v: Authenticated => Some(AuthenticatedInClientSideButNotServerSide(v.clientData, data, None))
    }

    /** If it's a valid transition, returns [[Some]]([[Authenticated]])), otherwise [[None]]. */
    def withAuthenticationSucceededOnServerSide(data: TServerSideAuthData): Option[ClientAuthState] = this match {
      case v: AuthenticatedInClientSide                 => Some(Authenticated(v.clientData, data))
      case v: AuthenticatedInClientSideButNotServerSide => Some(Authenticated(v.clientData, data))
      case v: Authenticated                             => Some(Authenticated(v.clientData, data))
      case _: Loading | _: NotAuthenticated             => None
    }
  }
  object ClientAuthState {

    /** Extracts the redirect page from the client auth state. */
    object WithRedirectE {
      def unapply(state: ClientAuthState): Option[Option[TPage]] = state match {
        case v: Loading                                   => Some(v.maybeRedirectToAfterRegistration)
        case v: NotAuthenticated                          => Some(v.maybeRedirectToAfterRegistration)
        case v: AuthenticatedInClientSide                 => Some(v.maybeRedirectToAfterRegistration)
        case v: AuthenticatedInClientSideButNotServerSide => Some(v.maybeRedirectToAfterRegistration)
        case _: Authenticated                             => None
      }
    }

    extension (signal: Signal[ClientAuthState]) {
      def splitByClientAuthState[Result](
        whenLoading: (Loading, Signal[Loading]) => Result,
        whenNotAuthenticated: (NotAuthenticated, Signal[NotAuthenticated]) => Result,
        whenAuthenticatedInClientSide: (AuthenticatedInClientSide, Signal[AuthenticatedInClientSide]) => Result,
        whenAuthenticatedInClientSideButNotServerSide: (
          AuthenticatedInClientSideButNotServerSide,
          Signal[AuthenticatedInClientSideButNotServerSide],
        ) => Result,
        whenAuthenticated: (Authenticated, Signal[Authenticated]) => Result,
      ): Signal[Result] =
        signal.splitEnum
          .handle(whenLoading)
          .handle(whenNotAuthenticated)
          .handle(whenAuthenticatedInClientSide)
          .handle(whenAuthenticatedInClientSideButNotServerSide)
          .handle(whenAuthenticated)
          .close
    }
  }

  trait AppPageForAuthenticatedDependencies {

    /** Shown while waiting for authentication to finish. */
    def pageLoadingIndicator: PageLoadingIndicator

    /** The current authentication state. */
    def authSignal: Signal[ClientAuthState]

    // /** Invoked when we want to set the redirect and save it to [[authStateRx]]. You probably want to use
    //   * [[ClientAuthState.withRedirectToAfterAuthenticationWithLogging]].
    //   */
    // def onSetRedirect(state: ClientAuthState, page: TPage): ClientAuthState
  }

  /** A page which is only available to users that has finished authenticating on the client side.
    *
    * Shows a [[PageLoadingIndicator]] while waiting for authentication to finish.
    *
    * @param renderForNotAuthenticated
    *   e.g. "Please log in"
    * @param renderForAuthenticatedInClientSideButNotServerSide
    *   e.g. "Welcome, please register"
    * @param renderForAuthenticated
    *   e.g. "Welcome back!"
    * @tparam PageData
    *   the type of the page data received from router
    */
  def AppPageForAuthenticatedInClientSide[PageData](
    pageDataToAppPage: PageData => TPage,
    renderForNotAuthenticated: (
      ClientAuthState.NotAuthenticated,
      Signal[ClientAuthState.NotAuthenticated],
      Signal[PageData],
    ) => PageRenderResult,
    renderForAuthenticatedInClientSideButNotServerSide: (
      ClientAuthState.AuthenticatedInClientSideButNotServerSide,
      Signal[ClientAuthState.AuthenticatedInClientSideButNotServerSide],
      Signal[PageData],
    ) => PageRenderResult,
    renderForAuthenticated: (
      ClientAuthState.Authenticated,
      Signal[ClientAuthState.Authenticated],
      Signal[PageData],
    ) => PageRenderResult,
  )(pageData: MaybeSignal[PageData])(using
    deps: AppPageForAuthenticatedDependencies
  ): PageRenderResult = {
    val dataSignal = pageData.deunionizeSignal
    val pageSignal = dataSignal.map(pageDataToAppPage)

    deps.authSignal
      .splitByClientAuthState(
        whenLoading = (_, _) => deps.pageLoadingIndicator.pageRenderResult,
        whenAuthenticatedInClientSide = (_, _) => deps.pageLoadingIndicator.pageRenderResult,
        whenAuthenticatedInClientSideButNotServerSide = (initial, signal) => {
          renderForAuthenticatedInClientSideButNotServerSide(initial, signal, dataSignal)
        },
        whenNotAuthenticated = (initial, signal) => renderForNotAuthenticated(initial, signal, dataSignal),
        whenAuthenticated = (initialAuth, authSignal) => renderForAuthenticated(initialAuth, authSignal, dataSignal),
      )
      .extract

    // TODO RESTORE: store the redirect when not authenticated
    // /** @see [[AppAuthenticatedPage.needToAuthenticateAndPossiblyRegister]] */
    // protected def needToAuthenticateAndPossiblyRegister(
    //   page: TPage,
    //   status: AppAuthenticatedPage.AuthStatus,
    // )(using appPageInit: AppPageInit): PageRenderResult = {
    //   appPageInit.setRedirectToOnRegistration(page)
    //   AppAuthenticatedPage.needToAuthenticateAndPossiblyRegister(status)
    // }
  }

  /** A page which is only available to users that has finished authenticating on the server side.
    *
    * Shows a [[PageLoadingIndicator]] while waiting for authentication to finish.
    *
    * @tparam PageData
    *   the type of the page data received from router
    */
  def AppPageForAuthenticatedInServerSide[PageData](
    pageDataToAppPage: PageData => TPage,
    renderForAuthenticationFailed: (
      ClientAuthState & ClientAuthHasFailedState,
      Signal[ClientAuthState & ClientAuthHasFailedState],
      Signal[PageData],
    ) => PageRenderResult,
    renderForAuthenticated: (
      ClientAuthState.Authenticated,
      Signal[ClientAuthState.Authenticated],
      Signal[PageData],
    ) => PageRenderResult,
  )(pageData: MaybeSignal[PageData])(using AppPageForAuthenticatedDependencies): PageRenderResult = {
    AppPageForAuthenticatedInClientSide(
      pageDataToAppPage,
      renderForNotAuthenticated = renderForAuthenticationFailed,
      renderForAuthenticatedInClientSideButNotServerSide = renderForAuthenticationFailed,
      renderForAuthenticated = renderForAuthenticated,
    )(pageData)
  }
}
