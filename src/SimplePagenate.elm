module SimplePager exposing (viewPager)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra as ListEx


viewPager :
  {
    currentPage : Int,
    totalPage : Int,
    pageRangeDisplayed : Int,
    customPreviousLabel : Maybe String,
    customNextLabel : Maybe String,
    customPageRangeLabel : Maybe String,
    breakLabel : Maybe String,
    pageClassName : Maybe String,
    previousClassName : Maybe String,
    nextClassName : Maybe String,
    breakClassName : Maybe String,
    onClickPager : Int -> msg
  } -> Html msg
viewPager
  {
    currentPage,
    totalPage,
    pageRangeDisplayed,
    customNextLabel,
    customPreviousLabel,
    customPageRangeLabel,
    breakLabel,
    onClickPager,
    pageClassName,
    previousClassName,
    nextClassName,
    breakClassName
  } =
    let
      pageButtonList = List.range 1 totalPage
                        |> List.map (\page ->
                                        if (page <= (currentPage + pageRangeDisplayed) && page >= (currentPage - pageRangeDisplayed)) ||
                                            page <= (1 + pageRangeDisplayed) ||
                                            page >= (totalPage - pageRangeDisplayed) then
                                          page
                                        else if page > (currentPage + pageRangeDisplayed) then
                                          -1
                                        else if page < (currentPage - pageRangeDisplayed) then
                                          -2
                                        else
                                          -9
                                    )
                        |> ListEx.unique
                        |> List.map (\page ->
                                        if -1 == page || -2 == page then
                                          span [ class <| Maybe.withDefault "" breakClassName ] [ text <| Maybe.withDefault "..." breakLabel ]
                                        else if -9 == page then
                                          text ""
                                        else if currentPage == page then
                                          button [ disabled True, onClick <| onClickPager page, class <| Maybe.withDefault "" pageClassName ] [ text <| String.fromInt page ]
                                        else
                                          button [ onClick <| onClickPager page, class <| Maybe.withDefault "" pageClassName ] [ text <| String.fromInt page ]
                                    )
    in
      span []
        <| List.append [ button [ onClick <| onClickPager <| (-) currentPage 1, disabled <| (==) currentPage 1, class <| Maybe.withDefault "" previousClassName ] [ text <| Maybe.withDefault "←" customPreviousLabel ] ]
        <| List.append pageButtonList
        <| List.singleton (button [ onClick <| onClickPager <| (+) currentPage 1, disabled <| (||) (totalPage < 1) <| (==) currentPage totalPage, class <| Maybe.withDefault "" nextClassName ] [ text <| Maybe.withDefault "→" customNextLabel ])
