# Bambik API reference

Generated from the module headers and doc comments — the single source of
truth for every combinator's contract (what its type cannot say: gating,
priming, echo protocols, container ownership). Regenerate after changing
any doc comment: `npm run api-docs`.

| Module | What lives there |
|---|---|
| [PUI](PUI.md) | the core type, its instances, and the pipeline combinators |
| [PUI.HTML](PUI.HTML.md) | the 1-1 HTML vocabulary: element oculars, leaves, collections |
| [PUI.SVG](PUI.SVG.md) | the SVG element oculars |
| [PUI.MDC](PUI.MDC.md) | the Material Design 2 components and oculars |
| [PUI.Web](PUI.Web.md) | the DOM carrier |
| [Data.Profunctor.Row](Data.Profunctor.Row.md) | the shared row-constraint floor |
| [Data.Profunctor.Row.RecordToRecord](Data.Profunctor.Row.RecordToRecord.md) | ×→× — editors: merge, lenses, Colens/feedback |
| [Data.Profunctor.Row.RecordToVariant](Data.Profunctor.Row.RecordToVariant.md) | ×→+ — events: merge, Resolving/Coresolving, Shutter/folding |
| [Data.Profunctor.Row.VariantToRecord](Data.Profunctor.Row.VariantToRecord.md) | +→× — statuses: merge, Retaining/Coretaining, Reel/unfolding |
| [Data.Profunctor.Row.VariantToVariant](Data.Profunctor.Row.VariantToVariant.md) | +→+ — dispatch: merge, prisms, Coprism/iterate |
| [Data.Profunctor.Row.Sequence](Data.Profunctor.Row.Sequence.md) | the sequence direction: keyed collections |

The narrative companion pieces: [why-bambik](../why-bambik.md) (the idea),
[row-profunctors](../row-profunctors.md) (the design note),
[type-errors](../type-errors.md) (reading the compile errors).
