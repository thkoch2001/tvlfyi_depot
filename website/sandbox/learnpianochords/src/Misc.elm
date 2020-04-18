module Misc exposing (..)

import Array exposing (Array)


comesAfter : a -> List a -> Maybe a
comesAfter x xs =
    case xs of
        [] ->
            Nothing

        _ :: [] ->
            Nothing

        y :: z :: rest ->
            if y == x then
                Just z

            else
                comesAfter x (z :: rest)


comesBefore : a -> List a -> Maybe a
comesBefore x xs =
    case xs of
        [] ->
            Nothing

        _ :: [] ->
            Nothing

        y :: z :: rest ->
            if z == x then
                Just y

            else
                comesBefore x (z :: rest)


find : (a -> Bool) -> List a -> Maybe a
find pred xs =
    case xs |> List.filter pred of
        [] ->
            Nothing

        x :: _ ->
            Just x
