# bambik on Diner — 2026-11-22, 51bba2b3

11th Gen Intel(R) Core(TM) i7-11850H @ 2.50GHz, 16 threads, 63 GB; Chrome/158.0.8127.36; node v25.6.0; 1311 s.

### View model in node: one feed's reads per station (ms)

| station    | 1×    | 4×     | 16×     | 64×    | growth |
| ---------- | ----- | ------ | ------- | ------ | ------ |
| Magazyn    | 0.44  | 1.909  | 7.765   | 36.424 | 1.06   |
| Sala       | 0.462 | 2.058  | 9.034   | 42.243 | 1.08   |
| Kuchnia    | 0.064 | 0.32   | 1.645   | 9.61   | 1.2    |
| Wydawka    | 0.03  | 0.118  | 0.517   | 2.554  | 1.07   |
| Raport     | 0.14  | 0.643  | 3.047   | 15.182 | 1.13   |
| Statystyki | 5.686 | 29.383 | 149.715 | 811.43 | 1.19   |

The costliest reads at the largest scale:

| read                         | 1×    | 4×    | 16×    | 64×     | growth |
| ---------------------------- | ----- | ----- | ------ | ------- | ------ |
| Statystyki: searchState      | 0.936 | 4.305 | 21.403 | 123.086 | 1.17   |
| Statystyki: soldAmountLine   | 0.84  | 4.333 | 22.012 | 117.843 | 1.19   |
| Statystyki: isLastPage       | 0.762 | 4.107 | 20.854 | 115.573 | 1.2    |
| Statystyki: soldQuantityLine | 0.83  | 4.385 | 22.121 | 113.63  | 1.18   |
| Statystyki: salesPage        | 0.771 | 4.144 | 21.157 | 113.578 | 1.2    |
| Statystyki: pageLine         | 0.774 | 3.99  | 20.688 | 113.349 | 1.2    |
| Statystyki: isFirstPage      | 0.757 | 4.058 | 21.217 | 113.317 | 1.2    |
| Sala: menuPage               | 0.176 | 0.903 | 4.288  | 22.202  | 1.16   |
| Magazyn: stockPage           | 0.191 | 0.927 | 3.974  | 19.737  | 1.11   |
| Raport: paidPage             | 0.059 | 0.306 | 1.627  | 9.199   | 1.21   |

Generating the day (`dayAt`): 1× 1.63 ms, 4× 6.41 ms, 16× 35.65 ms, 64× 87.4 ms.

### Responsiveness: what one doing costs the browser (ms)

Script, style and layout together; the split is the median at full speed. The budget is on the 95th percentile at a quarter of the speed: 100 ms for something done, 16 ms for a second passing, 1 s for the start.

| done                                     | n  | p50   | p95   | script / style / layout | p50 at ¼ | p95 at ¼ | budget   |
| ---------------------------------------- | -- | ----- | ----- | ----------------------- | -------- | -------- | -------- |
| stock: a portion more                    | 30 | 3.53  | 4.61  | 2.9 / 0.33 / 0.3        | 14.17    | 24.57    | within   |
| stock: a portion fewer                   | 30 | 3.55  | 4.57  | 2.87 / 0.36 / 0.32      | 14.93    | 19.68    | within   |
| stock: a key in a portion count          | 30 | 2.99  | 4.08  | 2.68 / 0 / 0.24         | 9.7      | 11.93    | within   |
| stock: a key in a price                  | 30 | 2.33  | 4.39  | 2.12 / 0 / 0.17         | 9.52     | 11.64    | within   |
| stock: a page turned                     | 30 | 19.11 | 25.73 | 12.14 / 4.66 / 2.58     | 80.82    | 101.27   | over 100 |
| stock: a group picked                    | 30 | 21.38 | 26.67 | 11.88 / 6.76 / 2.89     | 89.25    | 113.15   | over 100 |
| stock: a key in the search               | 32 | 3.57  | 18.45 | 3.19 / 0.25 / 0.24      | 14.33    | 69.83    | within   |
| hall: a dish added                       | 30 | 3.82  | 6.66  | 3.45 / 0.18 / 0.32      | 15.45    | 27.32    | within   |
| hall: a portion more on a line           | 30 | 5.33  | 6.54  | 4.35 / 0.56 / 0.45      | 20.03    | 24.88    | within   |
| hall: a portion fewer on a line          | 30 | 5.22  | 6.38  | 4.09 / 0.67 / 0.46      | 20.97    | 27.65    | within   |
| hall: a table picked                     | 30 | 5.93  | 7.22  | 4.31 / 1.12 / 0.86      | 23.27    | 28.6     | within   |
| hall: a page turned                      | 30 | 7.45  | 11.03 | 5.73 / 0.99 / 0.8       | 30.37    | 37.52    | within   |
| hall: a group picked                     | 30 | 9.56  | 13.34 | 6.58 / 2.16 / 0.89      | 35.16    | 49.63    | within   |
| hall: a key in the search                | 32 | 5.45  | 7.07  | 4.44 / 0.34 / 0.74      | 21.8     | 28.01    | within   |
| kitchen: a page turned                   | 30 | 14.49 | 19.77 | 9.33 / 3.11 / 1.8       | 58.26    | 72.99    | within   |
| kitchen: a line plated                   | 7  | 3.49  | 4.06  | 2.65 / 0.65 / 0.33      | 12.99    | 18.33    | within   |
| kitchen: an order passed on, asked       | 12 | 4.47  | 11.88 | 0.44 / 3.86 / 0.37      | 17.9     | 27.8     | within   |
| kitchen: an order passed on, confirmed   | 12 | 19.73 | 24.09 | 4.48 / 11.95 / 3.06     | 58.53    | 69.02    | within   |
| pass: a payment, asked                   | 10 | 2.77  | 6.6   | 0.51 / 1.96 / 0.37      | 10.6     | 18.24    | within   |
| pass: a payment, confirmed               | 10 | 11.37 | 12.91 | 2.12 / 7.36 / 1.69      | 31.82    | 40.96    | within   |
| report: a page turned                    | 30 | 3.81  | 5.62  | 2.58 / 0.71 / 0.61      | 13.55    | 18.52    | within   |
| statistics: a page turned                | 30 | 10.98 | 14.51 | 9.8 / 0.76 / 0.6        | 43.39    | 60.89    | within   |
| statistics: a group picked               | 30 | 14.63 | 21.83 | 12.22 / 1.88 / 0.58     | 58.52    | 67.99    | within   |
| statistics: a key in the search          | 32 | 12.56 | 18.04 | 12.1 / 0.14 / 0.55      | 50.1     | 69.53    | within   |
| app: a second passes at Magazyn          | 6  | 0.28  | 0.43  | 0.11 / 0.19 / 0         | 0.66     | 1.15     | within   |
| app: a second passes at Sala             | 6  | 0.4   | 0.44  | 0.16 / 0.24 / 0         | 0.87     | 1.41     | within   |
| app: a second passes at Kuchnia          | 6  | 0.77  | 1.02  | 0.62 / 0.21 / 0         | 1.08     | 2.25     | within   |
| app: a second passes at Wydawka          | 6  | 0.49  | 0.92  | 0.44 / 0.16 / 0         | 1.68     | 2.5      | within   |
| app: a second passes at Raport           | 6  | 0.44  | 0.51  | 0.29 / 0.15 / 0         | 0.84     | 1.14     | within   |
| app: a second passes at Statystyki       | 6  | 0.3   | 0.34  | 0.14 / 0.16 / 0         | 0.69     | 0.97     | within   |
| app: switched to Magazyn                 | 5  | 17.39 | 24.36 | 5.51 / 9.16 / 2.86      | 72.14    | 73.46    | within   |
| app: switched to Sala                    | 5  | 16.61 | 19.38 | 8.61 / 6.05 / 2.17      | 69.35    | 72.83    | within   |
| app: switched to Kuchnia                 | 5  | 10.68 | 12    | 3.58 / 5.33 / 1.71      | 49.96    | 58.87    | within   |
| app: switched to Wydawka                 | 5  | 6.18  | 7.98  | 2.32 / 2.93 / 1.05      | 34.89    | 67.55    | within   |
| app: switched to Raport                  | 5  | 5.12  | 6.08  | 2.39 / 2.17 / 0.59      | 23.06    | 24.96    | within   |
| app: switched to Statystyki              | 5  | 23.53 | 23.92 | 18.2 / 4.24 / 1.09      | 95.48    | 103.51   | over 100 |
| app: started, until it waits for its day | 6  | 46.06 | 54.73 | 40.47 / 2.09 / 3.85     | 182.53   | 186.54   | within   |
| app: started, the day drawn              | 6  | 55.48 | 60.72 | 42.04 / 4.09 / 10.06    | 199.66   | 207.15   | within   |

Where the time of a station's doings goes (CPU profile, self time by layer):

| station    | ms profiled | view model | carrier | design system | browser | gc |
| ---------- | ----------- | ---------- | ------- | ------------- | ------- | -- |
| Stock      | 1426        | 5%         | 20%     | 33%           | 38%     | 4% |
| Hall       | 853         | 16%        | 32%     | 8%            | 41%     | 4% |
| Kitchen    | 808         | 2%         | 16%     | 22%           | 56%     | 3% |
| Pass       | 314         | 1%         | 3%      | 17%           | 78%     | 1% |
| Report     | 98          | 6%         | 24%     | 7%            | 59%     | 4% |
| Statistics | 699         | 58%        | 9%      | 6%            | 22%     | 6% |

Stock, the functions that took most: (program) 538.69 ms; scrollToTab (node_modules/@material/web/tabs/internal/tabs.js) 74.73 ms; (garbage collector) 55.73 ms; showStringImpl (Data.Show) 34.12 ms; insertBefore 26.86 ms.
Hall, the functions that took most: (program) 332.39 ms; (garbage collector) 36.22 ms; (anonymous) (Effect) 31.9 ms; setAttribute 20.76 ms; __do3 (PUI) 19.96 ms.
Kitchen, the functions that took most: (program) 446.75 ms; animate 45.43 ms; showModal 35.21 ms; (garbage collector) 25.19 ms; (anonymous) (Effect) 14.29 ms.
Pass, the functions that took most: (program) 239.12 ms; showModal 17.63 ms; animate 15.86 ms; (anonymous) 3.31 ms; animateDialog (node_modules/@material/web/dialog/internal/dialog.js) 2.03 ms.
Report, the functions that took most: (program) 56.7 ms; setAttribute 4.15 ms; (garbage collector) 4.06 ms; (anonymous) (Effect) 1.55 ms; (anonymous) (Control.Bind) 1.14 ms.
Statistics, the functions that took most: (program) 145.55 ms; go (Data.Map.Internal) 90.59 ms; (anonymous) (Data.Map.Internal) 59.25 ms; (anonymous) (Diner) 52.11 ms; (anonymous) (Data.Ord) 48.23 ms.

No exception and no console warning.

### Large data: a station alone on days of growing load (ms)

The standard day is 1×; 64× is 19456 dishes, 2688 tables and 16128 orders. Growth is the exponent k of cost ≈ c·scale^k: 0 is flat, 1 linear, above 1 a finding.

| station    | measured        | 1×    | 4×     | 16×    | 64×     | growth |
| ---------- | --------------- | ----- | ------ | ------ | ------- | ------ |
| Stock      | drawn           | 79.63 | 97.54  | 141.38 | 287.03  | 0.3    |
| Stock      | a second passes | 0     | 0      | 0      | 0       | 0      |
| Stock      | a portion more  | 4.18  | 9.86   | 22.63  | 84.96   | 0.71   |
| Stock      | DOM nodes       | 6184  | 6192   | 6192   | 6192    | 0      |
| Stock      | heap (MB)       | 6.2   | 5.9    | 10.2   | 20.8    | 0.3    |
| Hall       | drawn           | 83.91 | 146.79 | 287.64 | 846.31  | 0.55   |
| Hall       | a second passes | 0.3   | 0.32   | 0.39   | 0.45    | 0.1    |
| Hall       | a dish added    | 4.66  | 9.56   | 28.86  | 109.44  | 0.76   |
| Hall       | DOM nodes       | 2457  | 5121   | 15777  | 54027   | 0.75   |
| Hall       | heap (MB)       | 7.2   | 11.7   | 18.8   | 51.6    | 0.46   |
| Kitchen    | drawn           | 68.86 | 77.5   | 103.32 | 176.58  | 0.22   |
| Kitchen    | a second passes | 0.27  | 0.27   | 0.34   | 0.26    | 0.01   |
| Kitchen    | a page turned   | 14.91 | 17.96  | 17.48  | 26.92   | 0.13   |
| Kitchen    | DOM nodes       | 3286  | 3143   | 3325   | 2896    | -0.02  |
| Kitchen    | heap (MB)       | 4.9   | 7.3    | 7.6    | 19.5    | 0.3    |
| Pass       | drawn           | 51.24 | 58.9   | 82     | 147.66  | 0.25   |
| Pass       | a second passes | 0.31  | 0.38   | 0.31   | 0.33    | 0      |
| Pass       | a payment taken | 15.1  | 16.01  | 18.84  | 24.42   | 0.12   |
| Pass       | DOM nodes       | 922   | 924    | 924    | 924     | 0      |
| Pass       | heap (MB)       | 4.7   | 4.7    | 8.1    | 12.7    | 0.25   |
| Report     | drawn           | 48    | 58.31  | 81.47  | 160.73  | 0.29   |
| Report     | a second passes | 0.23  | 0.32   | 0.3    | 0.35    | 0.09   |
| Report     | a page turned   | 4.32  | 4.49   | 6.18   | 17.55   | 0.33   |
| Report     | DOM nodes       | 301   | 301    | 301    | 301     | 0      |
| Report     | heap (MB)       | 4.4   | 5      | 7.6    | 20.7    | 0.37   |
| Statistics | drawn           | 77.92 | 153.46 | 404.31 | 1654.54 | 0.73   |
| Statistics | a second passes | 0     | 0      | 0      | 0       | 0      |
| Statistics | a page turned   | 12.19 | 37.62  | 156.64 | 802.94  | 1.01   |
| Statistics | DOM nodes       | 924   | 924    | 924    | 905     | 0      |
| Statistics | heap (MB)       | 5.3   | 8.7    | 26.4   | 32.2    | 0.47   |

Where drawing the day at 64× goes (CPU profile, self time by layer):

| station    | ms profiled | view model | carrier | design system | browser | gc  |
| ---------- | ----------- | ---------- | ------- | ------------- | ------- | --- |
| Stock      | 368         | 50%        | 20%     | 16%           | 8%      | 6%  |
| Hall       | 1040        | 20%        | 40%     | 14%           | 19%     | 8%  |
| Kitchen    | 229         | 56%        | 11%     | 11%           | 14%     | 8%  |
| Pass       | 187         | 54%        | 8%      | 11%           | 17%     | 10% |
| Report     | 211         | 63%        | 8%      | 7%            | 12%     | 9%  |
| Statistics | 1847        | 89%        | 1%      | 2%            | 2%      | 7%  |

With pagination off, everything listed is on screen. A row's cost is the difference to the paged station over the difference in rows:

| station    | scale | listed | drawn   | per row | a second passes | per row | DOM nodes |
| ---------- | ----- | ------ | ------- | ------- | --------------- | ------- | --------- |
| Stock      | 1×    | 304    | 672.29  | 2.016   | 0               | 0       | 144136    |
| Stock      | 4×    | 1216   | 2282.99 | 1.812   | 0               | 0       | 641133    |
| Hall       | 1×    | 346    | 158.55  | 0.267   | 0.33            | 0       | 6655      |
| Hall       | 4×    | 1384   | 470.97  | 0.272   | 0.31            | 0       | 22999     |
| Kitchen    | 1×    | 20     | 79.48   | 1.062   | 0.49            | 0.022   | 6085      |
| Kitchen    | 4×    | 80     | 179.35  | 1.455   | 1.4             | 0.016   | 22345     |
| Pass       | 1×    | 10     | 71.28   | —       | 0.49            | —       | 922       |
| Pass       | 4×    | 40     | 75.19   | 0.543   | 0.96            | 0.019   | 2422      |
| Report     | 1×    | 220    | 105.95  | 0.277   | 1.53            | 0.006   | 2598      |
| Report     | 4×    | 877    | 236.61  | 0.206   | 3.42            | 0.004   | 9825      |
| Statistics | 1×    | 263    | 137.08  | 0.235   | 0               | 0       | 3442      |
| Statistics | 4×    | 1040   | 363.04  | 0.204   | 0               | 0       | 11212     |

### Correctness under stress: a random walk against the view model

3000 steps of real input (seed 20261120, 166 s): after every one the station on screen read exactly what the view model says of the reference model. The day ended with 310 orders, 288 of them paid.

Done along the way: open ×1177, next page ×326, previous page ×239, add ×186, search ×162, order ×159, serve table ×136, plate ×125, stock + ×76, stock − ×64, price of ×61, group ×60, portions of ×53, line + ×48, zero ×45, send ×41, remove ×27, line − ×15.

No exception and no console warning.

### Races: the application driven faster than a person can

- holds: twelve clicks on a dish with no wait between them put twelve portions on the order
- holds: three digits typed while 4 seconds pass stay as typed, the caret after them
- holds: and are the stock when the stockroom is come back to
- holds: a dish clicked while the order was on its way went with it or stayed in stock, never both or neither
- holds: the last page losing its only row falls back to the page before
- holds: a payment confirmed twice at once is taken once

No exception and no console warning.

### Reliability: left running

9521 random clicks over 240 s under the clock, sampled every 25 after a forced collection. The slope is over the second half of the run:

|           | first sample | last sample | slope per minute |
| --------- | ------------ | ----------- | ---------------- |
| heap (MB) | 6.3          | 10          | 0.25             |
| DOM nodes | 7955         | 14899       | 280.8            |
| listeners | 872          | 1949        | 43.14            |

What one doing leaves behind for good (twenty of them between two forced collections, a station alone):

| done                      | DOM nodes | listeners | heap (KB) |
| ------------------------- | --------- | --------- | --------- |
| stockroom: a page turned  | -0.1      | 0         | 18.5      |
| hall: a page turned       | -0.1      | 0         | 15.5      |
| hall: a group picked      | 0         | 0         | 14.1      |
| hall: a table picked      | 0         | 0         | 4.7       |
| kitchen: a page turned    | -0.1      | 0         | 15.4      |
| report: a page turned     | -0.1      | 0         | 8.7       |
| statistics: a page turned | -0.1      | 0         | 8.1       |

900 station switches after a warm-up of twelve: heap 7.3 → 7.8 MB, DOM nodes 11623 → 11623, listeners 1275 → 1275.

The clock, watched for 60 s with the hall open: 1.001 of its seconds pass in a second. Across 7.2 s with the page frozen for 6 of them it counted 1.

No exception and no console warning.

### Application size: synthetic applications of growing width

Each station is a display, an editor, a keyed table of five rows and three buttons with their folds; one model, one knot. Compile time is for the two modules (view and view model), over the 0.3 s the compiler needs with nothing to do. Growth is the exponent against the number of stations.

|                                   | 6 stations | 12 stations | 24 stations | 48 stations | growth |
| --------------------------------- | ---------- | ----------- | ----------- | ----------- | ------ |
| compile (s)                       | 0.3        | 0.7         | 2.3         | 8.8         | 1.63   |
| bundle, minified (KB)             | 499.6      | 520.8       | 593.5       | 857.9       | 0.25   |
| bundle, gzipped (KB)              | 95.6       | 97.2        | 100.6       | 108.4       | 0.06   |
| drawn (ms)                        | 41.98      | 52.02       | 61.58       | 80.53       | 0.31   |
| DOM nodes                         | 966        | 1356        | 2136        | 3696        | 0.65   |
| a tick fed, one station open (ms) | 0.55       | 0.63        | 0.7         | 0.92        | 0.24   |
| a button clicked (ms)             | 1.51       | 1.36        | 1.47        | 1.44        | -0.01  |

No exception and no console warning.

