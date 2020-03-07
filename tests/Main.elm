module Main exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Test.Html.Query as Query exposing (Single, Multiple)
import Test.Html.Selector exposing (..)

import SimplePager

type Msg =
  ClickPager Int

suite : Test
suite =
    describe "Main Module Test"
      [ test "件数が1件の場合に、ページボタンが1つ表示される事" <|
          \_ ->
            SimplePager.viewPager {
                currentPage = 1,
                totalPage = 1,
                pageRangeDisplayed = 1,
                customPreviousLabel = Nothing,
                customNextLabel = Nothing,
                customBreakLabel = Nothing,
                pageClassName = Nothing,
                previousClassName = Nothing,
                nextClassName = Nothing,
                breakClassName = Nothing,
                onClickPager = ClickPager
              }
            |> Query.fromHtml
            |> Query.findAll [ tag "button" ]
            |> Query.count (Expect.equal 3),
        test "選択中のページが1ページ目の場合、戻るボタンが非活性である事" <|
          \_ ->
            SimplePager.viewPager {
                currentPage = 1,
                totalPage = 1,
                pageRangeDisplayed = 1,
                customPreviousLabel = Nothing,
                customNextLabel = Nothing,
                customBreakLabel = Nothing,
                pageClassName = Nothing,
                previousClassName = Nothing,
                nextClassName = Nothing,
                breakClassName = Nothing,
                onClickPager = ClickPager
              }
              |> Query.fromHtml
              |> Query.findAll [ tag "button", disabled True, containing [ text "←" ] ]
              |> Query.count (Expect.equal 1),
        test "選択中のページがラストページの場合、進むボタンが非活性である事" <|
          \_ ->
            SimplePager.viewPager {
                currentPage = 10,
                totalPage = 10,
                pageRangeDisplayed = 1,
                customPreviousLabel = Nothing,
                customNextLabel = Nothing,
                customBreakLabel = Nothing,
                pageClassName = Nothing,
                previousClassName = Nothing,
                nextClassName = Nothing,
                breakClassName = Nothing,
                onClickPager = ClickPager
              }
              |> Query.fromHtml
              |> Query.findAll [ tag "button", disabled True, containing [ text "→" ] ]
              |> Query.count (Expect.equal 1),
          test "選択中ページが1ページ目からレンジページ数以上だった場合、ブレークラベルが表示される事" <|
            \_ ->
              SimplePager.viewPager {
                currentPage = 3,
                totalPage = 10,
                pageRangeDisplayed = 2,
                customPreviousLabel = Nothing,
                customNextLabel = Nothing,
                customBreakLabel = Nothing,
                pageClassName = Nothing,
                previousClassName = Nothing,
                nextClassName = Nothing,
                breakClassName = Nothing,
                onClickPager = ClickPager
              }
              |> Query.fromHtml
              |> Query.find [ tag "span", containing [ text "..." ] ]
              |> Query.has [ tag "span", containing [ text "..." ] ],
          test "選択ページ数が最終ページからレンジページ数以下だった場合、ブレークラベルが表示される事" <|
            \_ ->
              SimplePager.viewPager {
                currentPage = 7,
                totalPage = 10,
                pageRangeDisplayed = 2,
                customPreviousLabel = Nothing,
                customNextLabel = Nothing,
                customBreakLabel = Nothing,
                pageClassName = Nothing,
                previousClassName = Nothing,
                nextClassName = Nothing,
                breakClassName = Nothing,
                onClickPager = ClickPager
              }
              |> Query.fromHtml
              |> Query.find [ tag "span", containing [ text "..." ] ]
              |> Query.has [ tag "span", containing [ text "..." ] ]
      ]
