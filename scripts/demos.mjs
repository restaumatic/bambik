// The demo registry: every demo, its directory under demo/, and the named
// module + entry function it is entered at. Single source of truth shared by
// the bundle scripts and the dev server — no demo is a `Main` module, so
// `spago bundle-app`'s fixed Main.main is never usable and each entry is
// synthesized from this table instead.

export const sets = {
  '7guis': {
    'counter': ['Counter', 'counter'],
    'temperature-converter': ['TemperatureConverter', 'temperatureConverter'],
    'flight-booker': ['FlightBooker', 'flightBooker'],
    'timer': ['Timer', 'timer'],
    'crud': ['Crud', 'crud'],
    'circle-drawer': ['CircleDrawer', 'circleDrawer'],
    'cells': ['Cells', 'cells'],
  },
  'nguis': {
    'order-form': ['OrderForm', 'orderForm'],
    'todomvc': ['TodoMvc', 'todoMvc'],
    'tip-calculator': ['TipCalculator', 'tipCalculator'],
    'quiz': ['Quiz', 'quiz'],
    'tic-tac-toe': ['TicTacToe', 'ticTacToe'],
    'markdown-previewer': ['MarkdownPreviewer', 'markdownPreviewer'],
    'helloworld': ['HelloWorld', 'helloWorld'],
    'calculator': ['Calculator', 'calculator'],
    'stopwatch': ['Stopwatch', 'stopwatch'],
    'shopping-cart': ['ShoppingCart', 'shoppingCart'],
    'password-generator': ['PasswordGenerator', 'passwordGenerator'],
    'color-mixer': ['ColorMixer', 'colorMixer'],
    'signup-form': ['SignupForm', 'signupForm'],
    'photo-gallery': ['PhotoGallery', 'photoGallery'],
    'inbox': ['Inbox', 'inbox'],
    'movie-browser': ['MovieBrowser', 'movieBrowser'],
    'weather': ['Weather', 'weather'],
    'auction': ['Auction', 'auction'],
    'checkout': ['Checkout', 'checkout'],
    'payment': ['Payment', 'payment'],
    'ticket-dispenser': ['TicketDispenser', 'ticketDispenser'],
    'potluck': ['Potluck', 'potluck'],
    'departures': ['Departures', 'departures'],
    'scoreboard': ['Scoreboard', 'scoreboard'],
    'reorder': ['Reorder', 'reorder'],
    'restaurant-menu': ['RestaurantMenu', 'restaurantMenu'],
    'espresso-bar': ['EspressoBar', 'espressoBar'],
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
