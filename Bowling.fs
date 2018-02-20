module Bowling

type NbPins = private NbPins of int

module NbPins = 
    let from char =
        match char with
        | '-' -> NbPins 0
        | _ ->
            let i = int char - 48
            if i < 0 || i > 9 then 
                failwithf "%c is not a valid nb pins" char
            NbPins i
    let value (NbPins nb) = nb        

type Frame =
    | Strike
    | Spare of firstRoll:NbPins
    | Roll of firstRoll:NbPins * secondRoll:NbPins

type LastFrame =
    | LastStrike of firstBonusBall:BonusBallNbPins * secondBonusBall:BonusBallNbPins
    | LastSpare of firstRoll:NbPins * bonusBall:BonusBallNbPins
    | LastRoll of firstRoll:NbPins * secondRoll:NbPins
and BonusBallNbPins = NbPins of NbPins | All

type Game = private Game of Frame list * LastFrame

module Game = 
    let private bonusBallNbPinsFrom = function
        | 'X' -> All
        | nb -> NbPins <| NbPins.from nb
    let rec private create' frames lastFrame = function
        | ['X'; 'X'; 'X'] ->
            frames, Some <| LastStrike (All, All)
        | [firstRoll; '/'; nb] ->
            frames, Some <| LastSpare (NbPins.from firstRoll, bonusBallNbPinsFrom nb)
        | [firstRoll; secondRoll] -> 
            frames, Some <| LastRoll (NbPins.from firstRoll, NbPins.from secondRoll)
        | 'X'::tail ->
            let frames = frames @ [Strike]
            create' frames lastFrame tail
        | firstRoll::'/'::tail ->
            let frames = frames @ [Spare <| NbPins.from firstRoll]
            create' frames lastFrame tail
        | firstRoll::secondRoll::tail -> 
            let frames = frames @ [Roll (NbPins.from firstRoll, NbPins.from secondRoll)] 
            create' frames lastFrame tail
        | _ -> failwith "Unknown rolls"
    let create array =
        let (frames, Some lastFrame) = create' [] None array
        frames, lastFrame

let toInt' = function
    | NbPins nb -> NbPins.value nb
    | All -> 10

let score game =
    let frames, lastFrame = game
    let lastFrameScore, lastFrameFirstBall, lastFrameSecondBall =
        match lastFrame with
        | LastStrike (bonus1, bonus2) -> 10 + toInt' bonus1 + toInt' bonus2, 10, toInt' bonus1
        | LastSpare (fst, bonus) -> 10 + toInt' bonus, NbPins.value fst, 10 - NbPins.value fst 
        | LastRoll (fst, snd) -> NbPins.value fst + NbPins.value snd, NbPins.value fst, NbPins.value snd 
    let rec score' acc = function
        | Strike::tail -> 
            let nextRoll, nextNextRoll, nextFrames =
                match tail with
                | Strike::Strike::_ -> 10, 10, tail
                | Strike::Spare fst::_
                | Strike::Roll (fst, _)::_ -> 10, NbPins.value fst, tail
                | [Strike] -> 10, lastFrameFirstBall, []
                | Spare _::_ -> 10, 0, tail
                | Roll (fst, snd)::_ -> NbPins.value fst, NbPins.value snd, tail
                | [] -> lastFrameFirstBall, lastFrameSecondBall, tail
            score' (acc + 10 + nextRoll + nextNextRoll) nextFrames            
        | Spare _::tail ->
            let nextRoll =
                match tail with
                | Strike::_ -> 10
                | Spare fst::_
                | Roll (fst, _)::_ -> NbPins.value fst
                | [] -> lastFrameFirstBall
            score' (acc + 10 + nextRoll) tail            
        | Roll (fst, snd)::tail -> score' (acc + NbPins.value fst + NbPins.value snd) tail
        | [] -> acc
    lastFrameScore + score' 0 frames

let game = Game.create ['X';'X';'X';'X';'X';'X';'X';'X';'X';'X';'X';'X']
let game = Game.create ['9';'-';'9';'-';'9';'-';'9';'-';'9';'-';'9';'-';'9';'-';'9';'-';'9';'-';'9';'-']
let game = Game.create ['5';'/';'5';'/';'5';'/';'5';'/';'5';'/';'5';'/';'5';'/';'5';'/';'5';'/';'5';'/';'5']
let game = Game.create ['X'; 'X'; '3'; '5';'3'; '3']

// Simplest impl https://blogs.msdn.microsoft.com/ashleyf/2010/11/22/bowling-kata/
let rec score' acc = function 
    | 10 :: (a :: b :: _ as t) -> score' (acc + 10 + a + b) t // strike 
    | 10 :: t -> score' acc t // final frames following strike 
    | a :: b :: [c] when a + b = 10 -> acc + 10 + c // final frame spare
    | a :: b :: (c :: _ as t) when a + b = 10 -> score' (acc + 10 + c) t // spare 
    | h :: t -> score' (acc + h) t // normal roll 
    | [] -> acc
