// randomElementId :: Effect String
export function randomElementId() {
  return "" + Math.floor(Math.random() * 99999999 + 100000000); // TODO use UUID?
}
