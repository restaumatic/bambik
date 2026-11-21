// Summaries of samples and the fits the report prints.
export const quantile = (xs, q) => {
  if (!xs.length) return NaN
  const s = [...xs].sort((a, b) => a - b)
  const i = (s.length - 1) * q
  const lo = Math.floor(i), hi = Math.ceil(i)
  return s[lo] + (s[hi] - s[lo]) * (i - lo)
}

export const summary = xs => ({
  n: xs.length,
  p50: round(quantile(xs, 0.5)),
  p95: round(quantile(xs, 0.95)),
  max: round(Math.max(...xs)),
})

export const round = (x, digits = 2) => Number.isFinite(x) ? +x.toFixed(digits) : x

// least squares of y on x: slope and intercept
export const line = (xs, ys) => {
  const n = xs.length, mx = xs.reduce((a, b) => a + b, 0) / n, my = ys.reduce((a, b) => a + b, 0) / n
  const sxx = xs.reduce((a, x) => a + (x - mx) ** 2, 0)
  const slope = sxx === 0 ? 0 : xs.reduce((a, x, i) => a + (x - mx) * (ys[i] - my), 0) / sxx
  return { slope, intercept: my - slope * mx }
}

// the exponent k of y ≈ c·x^k, fitted on logarithms
export const exponent = (xs, ys) => round(line(xs.map(Math.log), ys.map(y => Math.log(Math.max(y, 1e-6)))).slope)

export const table = (head, rows) => {
  const cells = [head, ...rows].map(r => r.map(c => String(c)))
  const w = head.map((_, i) => Math.max(...cells.map(r => r[i].length)))
  const fmt = r => '| ' + r.map((c, i) => c.padEnd(w[i])).join(' | ') + ' |'
  return [fmt(cells[0]), '| ' + w.map(n => '-'.repeat(n)).join(' | ') + ' |', ...cells.slice(1).map(fmt)].join('\n')
}
