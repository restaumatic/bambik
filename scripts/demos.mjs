// The demo registry: every demo, its directory under demo/, and the named
// module + entry function it is entered at. Single source of truth shared by
// the bundle scripts and the dev server — no demo is a `Main` module, so
// `spago bundle-app`'s fixed Main.main is never usable and each entry is
// synthesized from this table instead.

export const sets = {
  '7guis': {
    'counter-mdc2': ['CounterMDC2', 'counterMDC2'],
    'temperature-converter-mdc2': ['TemperatureConverterMDC2', 'temperatureConverterMDC2'],
    'flight-booker-mdc2': ['FlightBookerMDC2', 'flightBookerMDC2'],
    'timer-mdc2': ['TimerMDC2', 'timerMDC2'],
    'crud-mdc2': ['CrudMDC2', 'crudMDC2'],
    'circle-drawer-mdc2': ['CircleDrawerMDC2', 'circleDrawerMDC2'],
    'cells-mdc2': ['CellsMDC2', 'cellsMDC2'],
    // MDC3 (PUI.MDC3) siblings of the MDC2 demos — same app, import switched
    'counter-mdc3': ['CounterMDC3', 'counterMDC3'],
    'temperature-converter-mdc3': ['TemperatureConverterMDC3', 'temperatureConverterMDC3'],
    'flight-booker-mdc3': ['FlightBookerMDC3', 'flightBookerMDC3'],
    'timer-mdc3': ['TimerMDC3', 'timerMDC3'],
    'crud-mdc3': ['CrudMDC3', 'crudMDC3'],
    'circle-drawer-mdc3': ['CircleDrawerMDC3', 'circleDrawerMDC3'],
    'cells-mdc3': ['CellsMDC3', 'cellsMDC3'],
  },
  'nguis': {
    'order-form-mdc2': ['OrderFormMDC2', 'orderFormMDC2'],
    'todomvc-mdc2': ['TodoMvcMDC2', 'todoMvcMDC2'],
    'tip-calculator-mdc2': ['TipCalculatorMDC2', 'tipCalculatorMDC2'],
    'quiz-mdc2': ['QuizMDC2', 'quizMDC2'],
    'tic-tac-toe-mdc2': ['TicTacToeMDC2', 'ticTacToeMDC2'],
    'markdown-previewer-mdc2': ['MarkdownPreviewerMDC2', 'markdownPreviewerMDC2'],
    'helloworld': ['HelloWorld', 'helloWorld'],
    'calculator-mdc2': ['CalculatorMDC2', 'calculatorMDC2'],
    'stopwatch-mdc2': ['StopwatchMDC2', 'stopwatchMDC2'],
    'shopping-cart-mdc2': ['ShoppingCartMDC2', 'shoppingCartMDC2'],
    'password-generator-mdc2': ['PasswordGeneratorMDC2', 'passwordGeneratorMDC2'],
    'color-mixer-mdc2': ['ColorMixerMDC2', 'colorMixerMDC2'],
    'signup-form-mdc2': ['SignupFormMDC2', 'signupFormMDC2'],
    'photo-gallery-mdc2': ['PhotoGalleryMDC2', 'photoGalleryMDC2'],
    'inbox-mdc2': ['InboxMDC2', 'inboxMDC2'],
    'movie-browser-mdc2': ['MovieBrowserMDC2', 'movieBrowserMDC2'],
    'weather-mdc2': ['WeatherMDC2', 'weatherMDC2'],
    'auction-mdc2': ['AuctionMDC2', 'auctionMDC2'],
    'checkout-mdc2': ['CheckoutMDC2', 'checkoutMDC2'],
    'payment-mdc2': ['PaymentMDC2', 'paymentMDC2'],
    'ticket-dispenser-mdc2': ['TicketDispenserMDC2', 'ticketDispenserMDC2'],
    'cashbox-mdc2': ['CashboxMDC2', 'cashboxMDC2'],
    'parcel-mdc2': ['ParcelMDC2', 'parcelMDC2'],
    'potluck-mdc2': ['PotluckMDC2', 'potluckMDC2'],
    'departures-mdc2': ['DeparturesMDC2', 'departuresMDC2'],
    'scoreboard-mdc2': ['ScoreboardMDC2', 'scoreboardMDC2'],
    'reorder-mdc2': ['ReorderMDC2', 'reorderMDC2'],
    'restaurant-menu': ['RestaurantMenu', 'restaurantMenu'],
    'espresso-bar-mdc2': ['EspressoBarMDC2', 'espressoBarMDC2'],
    'espresso-bar-mdc3': ['EspressoBarMDC3', 'espressoBarMDC3'],
    'order-dashboard-mdc3': ['OrderDashboardMDC3', 'orderDashboardMDC3'],
    // non-Material design systems — one showcase each (PUI.Shoelace,
    // PUI.Fluent, PUI.Bootstrap), proving the vocabularies interchangeable
    'product-review-shoelace': ['ProductReviewShoelace', 'productReviewShoelace'],
    'meeting-booker-fluent': ['MeetingBookerFluent', 'meetingBookerFluent'],
    'loan-calculator-bootstrap': ['LoanCalculatorBootstrap', 'loanCalculatorBootstrap'],
    // MDC3 (PUI.MDC3) siblings of the MDC2 demos — same app, import switched
    'order-form-mdc3': ['OrderFormMDC3', 'orderFormMDC3'],
    'todomvc-mdc3': ['TodoMvcMDC3', 'todoMvcMDC3'],
    'tip-calculator-mdc3': ['TipCalculatorMDC3', 'tipCalculatorMDC3'],
    'quiz-mdc3': ['QuizMDC3', 'quizMDC3'],
    'tic-tac-toe-mdc3': ['TicTacToeMDC3', 'ticTacToeMDC3'],
    'markdown-previewer-mdc3': ['MarkdownPreviewerMDC3', 'markdownPreviewerMDC3'],
    'calculator-mdc3': ['CalculatorMDC3', 'calculatorMDC3'],
    'stopwatch-mdc3': ['StopwatchMDC3', 'stopwatchMDC3'],
    'shopping-cart-mdc3': ['ShoppingCartMDC3', 'shoppingCartMDC3'],
    'password-generator-mdc3': ['PasswordGeneratorMDC3', 'passwordGeneratorMDC3'],
    'color-mixer-mdc3': ['ColorMixerMDC3', 'colorMixerMDC3'],
    'signup-form-mdc3': ['SignupFormMDC3', 'signupFormMDC3'],
    'photo-gallery-mdc3': ['PhotoGalleryMDC3', 'photoGalleryMDC3'],
    'inbox-mdc3': ['InboxMDC3', 'inboxMDC3'],
    'movie-browser-mdc3': ['MovieBrowserMDC3', 'movieBrowserMDC3'],
    'weather-mdc3': ['WeatherMDC3', 'weatherMDC3'],
    'auction-mdc3': ['AuctionMDC3', 'auctionMDC3'],
    'checkout-mdc3': ['CheckoutMDC3', 'checkoutMDC3'],
    'payment-mdc3': ['PaymentMDC3', 'paymentMDC3'],
    'ticket-dispenser-mdc3': ['TicketDispenserMDC3', 'ticketDispenserMDC3'],
    'cashbox-mdc3': ['CashboxMDC3', 'cashboxMDC3'],
    'parcel-mdc3': ['ParcelMDC3', 'parcelMDC3'],
    'potluck-mdc3': ['PotluckMDC3', 'potluckMDC3'],
    'departures-mdc3': ['DeparturesMDC3', 'departuresMDC3'],
    'scoreboard-mdc3': ['ScoreboardMDC3', 'scoreboardMDC3'],
    'reorder-mdc3': ['ReorderMDC3', 'reorderMDC3'],
  },
}

// Every demo as { set, name, dir, mod, fn } — dir is repo-relative, and also
// the path the page is served at under the deployed /bambik/demo/ tree.
export const all = Object.entries(sets).flatMap(([set, demos]) =>
  Object.entries(demos).map(([name, [mod, fn]]) => ({
    set, name, mod, fn, dir: `demo/${set}/${name}`,
  })))

export const inSet = set => all.filter(d => d.set === set)

export const entryFor = ({ mod, fn }) =>
  `import { ${fn} } from './output/${mod}/index.js'; ${fn}();`
