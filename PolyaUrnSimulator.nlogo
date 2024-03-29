extensions [ rnd table csv]


globals [
  total-urns ; track the total number of urns
  initial-urns ; the number of urns to start with should be 2
  counter ; counter to keep track of iterations
  caller-turtle-id ; the turtle id of the caller agent
  caller-urn-id ; the urn id of the caller agent
  caller-memory-buffer
  callee-turtle-id ; the turtle id of the callee agent
  callee-urn-id ; the urn id of the callee agent
  callee-memory-buffer
  interacted-urns
  dict
  size-adj-possible
  distance-between
  colour
  history
  gamma
]

breed [urns urn]

urns-own [
 possible-interactions ;; the urns available to be interacted with
 number-possible-interactions ;; the size of the urn, or the number of agents this agent has interacted with in the past
 past-interactions ;; a list of the urns previously interacted with
 memory-buffer ;; the memory buffer of the urn
]

;;;
;;; SETUP PROCEDURES
;;;

to setup
  clear-all
  set dict table:make
  set history [["caller" "callee"]]

  set-default-shape urns "circle"
  ask patches [ set pcolor white ]
  set colour turquoise

  ;; step 1 - initialise two urns, each with a memory buffer
  set initial-urns 2
  set total-urns 2
  set counter 0
  while [ counter < initial-urns ] [
    set total-urns ( total-urns + nu + 1)
    create-urns 1 [
    set size 1
    set label who
    table:put dict who who
    set color colour
    set possible-interactions (range (total-urns - (( 1 ) * (nu + 1))) total-urns)
    set number-possible-interactions (length possible-interactions)
    set memory-buffer possible-interactions
    set past-interactions []
    (ifelse
        who = 0 [
          set possible-interactions ( insert-item 0 possible-interactions 1)
          ;;set past-interactions [1]
        ]
        who = 1 [
          set possible-interactions ( insert-item 0 possible-interactions 0)
          ;;set past-interactions [0]
        ]
    )
    ]
    set counter counter + 1
  ]

  set size-adj-possible ((2 * (nu + 1)) + 2)

  ask turtle 0 [ create-link-with turtle 1 [ set color grey ] ]
  set interacted-urns []
  set interacted-urns ( insert-item 0 interacted-urns 0 )
  set interacted-urns ( insert-item 0 interacted-urns 1 )
  reset-ticks
end

;;;
;;; GO PROCEDURES
;;;

to go
  if ticks >= num-iterations [stop] ; limit number of iterations

  ;; step 2 - extract balls from urn
  get-caller
  set caller-urn-id table:get dict caller-turtle-id ; update the dictionary
  get-callee

  if member? callee-urn-id interacted-urns [ ; check if callee has interacted before
    ask turtles with [label = callee-urn-id] [
      set callee-turtle-id who
    ]
  ]

  if not member? callee-urn-id interacted-urns [ ; check if callee has interacted before
    create-urns 1　[
      ; set the spawn position of new agents
      if positioning = "Border" [
        let x 0
        let y 0
        let side random 4
        if side = 0 [ set x -32 set y random-ycor ]
        if side = 1 [ set x 32 set y random-ycor ]
        if side = 2 [ set x random-xcor set y -32 ]
        if side = 3 [ set x random-xcor set y 32 ]

        setxy x y
      ]
      if positioning = "Circle" [
        rt random-float 360
      ]

      if positioning = "Random" [
        setxy (random-xcor) (random-ycor)
      ]

      set size 1
      set callee-turtle-id who
      table:put dict who callee-urn-id
      set label callee-urn-id
      set color colour
      set possible-interactions (range (total-urns) (total-urns + (( 1 ) * (nu + 1)))) ; expand adjacent possible space
      set number-possible-interactions (length possible-interactions)
      set memory-buffer possible-interactions ; update the memory buffer object
      set past-interactions []
      set total-urns (total-urns + nu + 1 + 1)
    ]
    set interacted-urns ( insert-item 0 interacted-urns callee-urn-id )
    set size-adj-possible (size-adj-possible + (nu + 1))
  ]
  set history lput (list caller-turtle-id callee-turtle-id) history ; update the history
  ask turtle caller-turtle-id [ create-link-with turtle callee-turtle-id [ set color grey ] ] ; create the link in the network

  ;; step 4 - novelty
  ask turtles with [label = caller-urn-id] [
    if not member? callee-turtle-id past-interactions [
      ;; update the memory buffer
      if strategy = "WSW"[
        let updated-memory-buffer wsw possible-interactions
        set memory-buffer updated-memory-buffer
        foreach updated-memory-buffer [
          aid ->
          if aid != label [
            set possible-interactions (insert-item 0 possible-interactions aid)
          ]
        ]
      ]
      if strategy = "SSW"[
        let updated-memory-buffer ssw memory-buffer caller-urn-id
        set memory-buffer updated-memory-buffer
        foreach updated-memory-buffer [
          aid ->
          if aid != label [
            set possible-interactions (insert-item 0 possible-interactions aid)
          ]
        ]
      ]
    ]
  ]
  ask turtles with [label = callee-urn-id] [
    if not member? callee-urn-id past-interactions [

      ;; update the memory buffer
      if strategy = "WSW"[
        let updated-memory-buffer wsw possible-interactions
        set memory-buffer updated-memory-buffer
        foreach updated-memory-buffer [
          aid ->
          if aid != label [
            set possible-interactions (insert-item 0 possible-interactions aid)
          ]
        ]
      ]
      if strategy = "SSW"[
        let updated-memory-buffer ssw memory-buffer callee-urn-id
        set memory-buffer updated-memory-buffer
        foreach updated-memory-buffer [
          aid ->
          if aid != label [
            set possible-interactions (insert-item 0 possible-interactions aid)
          ]
        ]
      ]
    ]
  ]

  ;; step 3 - reinforcement
  ask turtles with [label = caller-urn-id] [
    set counter 0
    while [ counter < rho ] [
      set past-interactions (insert-item 0 past-interactions callee-urn-id)
      set counter (counter + 1)
    ]
    set number-possible-interactions (length possible-interactions)
  ]
  ask turtles with [label = callee-urn-id] [
    set counter 0
    while [ counter < rho ] [
      set past-interactions (insert-item 0 past-interactions caller-urn-id)
      set counter (counter + 1)
    ]
    set number-possible-interactions (length possible-interactions)
  ]


  ;; make the interacting agents move closer together
  ask urn caller-turtle-id [
      face urn callee-turtle-id
      forward 1
  ]
  ask urn callee-turtle-id [
      face urn caller-turtle-id
      forward 1
  ]

  calc_gamma
  tick

end

;get the caller agent
to get-caller
  ask rnd:weighted-one-of urns [ number-possible-interactions ] [
   set caller-turtle-id who
   set caller-memory-buffer memory-buffer
  ]
end

; get the callee agent
to get-callee
  ask turtle caller-turtle-id [
    set callee-urn-id (one-of possible-interactions)
    set callee-memory-buffer memory-buffer
  ]
end

;; wsw strategy
to-report wsw [agents-based-interactions]
  let updated-memory-buffer rnd:weighted-n-of-list (nu + 1) agents-based-interactions [ [w] -> w ]
  report updated-memory-buffer
end

;; ssw strategy
to-report ssw [previous-memory-buffer aid]
  let updated-memory-buffer ( insert-item 0 (remove-item nu previous-memory-buffer) aid)
  report updated-memory-buffer
end

; adjust the layout of the network
to layout
  layout-spring (turtles with [any? link-neighbors]) links 0.4 ((scale / 1.5) + 1) ((scale * 0.3) + 1)
end

to resize-nodes
    ask turtles [ set size (  (count link-neighbors) ^ (scale / 10.0 ) ) ]
end

; save the network image to computer
to save-network
  let filename word " " ( word "rho" word (rho) word "nu" word (nu) "network.jpg")
  export-view filename ; Save the current view as an image
end

;; calculate the Heaps' Law Exponent
to calc_gamma
  ifelse ( (ticks > 1) and ((count links) > 2) ) [
  set gamma (log (ticks) (count links))
] [
  set gamma 0
]

end
@#$#@#$#@
GRAPHICS-WINDOW
737
29
1347
640
-1
-1
2.0
1
1
1
1
1
0
0
0
1
-150
150
-150
150
1
1
1
ticks
30.0

SLIDER
21
33
193
66
rho
rho
1
30
8.0
1
1
NIL
HORIZONTAL

SLIDER
21
80
193
113
nu
nu
0
20
20.0
1
1
NIL
HORIZONTAL

CHOOSER
41
123
179
168
strategy
strategy
"SSW" "WSW"
1

BUTTON
20
228
90
261
SETUP
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
103
272
193
305
GO ONCE
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
20
271
89
304
GO
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

PLOT
0
454
200
604
Size Adj. Possible Space
Time
# of agents
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot size-adj-possible"

MONITOR
32
405
201
450
Size Adj. Possible Space   
size-adj-possible
17
1
11

CHOOSER
246
33
384
78
positioning
positioning
"Random" "Border" "Circle"
0

BUTTON
273
88
356
121
LAYOUT
layout\nresize-nodes
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
4
666
204
816
Degree Distribution
degree
# of nodes
1.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "\nlet max-degree max [count link-neighbors] of turtles\nplot-pen-reset  ;; erase what we plotted before\nset-plot-x-range 1 (max-degree + 1)  ;; + 1 to make room for the width of the last bar\nhistogram [count link-neighbors] of turtles"

PLOT
222
454
422
604
Number Interacted Agents
Time
# of agents
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count turtles"

MONITOR
235
404
424
449
Number Interacted Agents    
count turtles
0
1
11

MONITOR
29
616
152
661
Max Num of Links
max [count link-neighbors] of turtles
17
1
11

TEXTBOX
31
10
226
28
Simulation Settings
16
0.0
1

TEXTBOX
230
10
434
50
Visualisation Settings
16
0.0
1

SLIDER
229
134
401
167
scale
scale
0
15
12.0
1
1
NIL
HORIZONTAL

PLOT
224
667
424
817
Degree Distribution (log-log)
log(degree)
log(# of nodes)
0.0
0.3
0.0
0.3
true
false
"" ""
PENS
"default" 1.0 2 -16777216 true "" "let max-degree max [count link-neighbors] of turtles\n;; for this plot, the axes are logarithmic, so we can't\n;; use \"histogram-from\"; we have to plot the points\n;; ourselves one at a time\nplot-pen-reset  ;; erase what we plotted before\n;; the way we create the network there is never a zero degree node,\n;; so start plotting at degree one\nlet degree 1\nwhile [degree <= max-degree] [\n  let matches turtles with [count link-neighbors = degree]\n  if any? matches\n    [ plotxy log degree 10\n             log (count matches) 10 ]\n  set degree degree + 1\n]"

SLIDER
23
179
195
212
num-iterations
num-iterations
1
5000
1000.0
1
1
NIL
HORIZONTAL

BUTTON
246
226
400
259
Save Network Image
save-network
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
257
272
392
305
Save History CSV
csv:to-file \"history.csv\" history
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
33
340
123
385
Caller Agent
caller-urn-id
0
1
11

MONITOR
137
340
227
385
Callee Agent
callee-urn-id
17
1
11

PLOT
447
456
647
606
Number Edges
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count links"

MONITOR
452
404
652
449
Number Edges
count links
17
1
11

PLOT
449
668
649
818
Heaps' Law Exponent
NIL
NIL
0.0
2.0
0.0
2.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot gamma\n\n"

MONITOR
466
615
651
660
Heaps' Law Exponent             
gamma
2
1
11

@#$#@#$#@
## WHAT IS IT?

This project impements the modified Polya's urn model for modelling the growth and dyanmics of novelties in social networks. In particular, this modelling strategy makes use of the adjacent possible space used in biology, to expand social networks based on their possible next interacations.

## HOW IT WORKS

In this modified polya's urn model model, each agent (or person) is represented by an urn. Inside each agents urn is a number of balls with different IDs on them. These IDs represent the other agents that this agent can interact with. When the model is initialised, two urns are created, each filled with nu + 1 initial interactale agents. These balls represent the agent's possible space - a term coined in biology that represents possible interactions that have not yet happened. 

At each time step, we select an urn at random, weighted by the number of balls in this urn. This selection represents prefential attachment, that is, urns with more interactions, and hence more balls, tend to take part in interactions more frequently. This selected agent is known as the caller agent. We then select a callee agent by selecting a ball from the caller agents urn. These two agents are then said to interact. 

The first step is to check if this interaction is a novel interaction, meaning that it has not happened before. If this is the case, then we exchange each agents so called memory buffer. The memory buffer is a set of nu + 1 balls from each agents urn that represents the agents they will share with the other agents. This exchange of connections allows callee to interact with some of the callees past connections and vice versa. In this way, the agent possible space of each agent gets bigger. The ids that are in each agents memory buffer are determined by an exchange strategy s, which in this case is either WSW (each agent exchanges their most commonly interacted connections) or SSW (each agent exchanges their most recent interactions).

The next step is reinforcement - that is, into the callee agents urn we place rho copies of the callers ball and vice versa. In this way, we strengthen the two agents relationship, increasing the likelyhood of this interaction occuring again in the future.

In this way, the model is entirely defined by three parameters: rho, the reinforcement value, nu, the size of the memory buffer to be exchanged and s, the strategy used for deciding the memory buffer.

## HOW TO USE IT

To use the model, select the values of rho, nu and s. You can also select how the agents are positioned - whether they begin on a the world border, in a circle, or randomly.

Then select set up and go. You can go in single time steps or you can loop forever.

## THINGS TO NOTICE

The graphs represent the expanse of the adjacent possible space. As nu is increased, we can see that the adjacent possible space grows faster.

## THINGS TO TRY

Try to run the model with different positioning values and see how the networks form.

## EXTENDING THE MODEL

Try to add different exchange strategies to see how the network expands.

Try to update how the agents are positioned initially, and how they move at each interaction.

## NETLOGO FEATURES

One benefit of using NetLogo is how we can visualise the growth of the social network using both the view of the world and the graphs.

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.3.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
