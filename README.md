# elm-simple-pagenate
A elm simple pagenater.

## Usage

```elm
import SimplePager

type Msg =
  ClickPager Int

view model =
  div [] [
    SimplePager.viewPager {
      currentPage = model.currentPage,
      totalPage = model.totalPage,
      pageRangeDisplayed = 2,
      customPreviousLabel = Nothing,
      customNextLabel = Nothing,
      customPageRangeLabel = Nothing,
      breakLabel = Nothing,
      pageClassName = Nothing,
      previousClassName = Nothing,
      nextClassName = Nothing,
      breakClassName = Nothing,
      onClickPager = ClickPager
    }
  ]


```

### image
![search](./docs/image/usageSample1.gif)
