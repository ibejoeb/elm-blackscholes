module BlackScholes exposing ( callPrice, putPrice ) 


nprime : Float -> Float
nprime x =
    (e ^ -(x^2 / 2)) / sqrt(2 * pi)


p : Float -> Float
p x =
    let
        ( p, b1, b2, b3, b4, b5 ) = 
            ( 0.2316419, 0.319381530, -0.356563782, 1.781477937, -1.821255978, 1.330274429 )
        t  = 1 / (1 + p * abs(x))
        t1 = b1 * t
        t2 = b2 * t^2
        t3 = b3 * t^3
        t4 = b4 * t^4
        t5 = b5 * t^5
        cd = 1 - (nprime x) * (t1 + t2 + t3 + t4 + t5)
    in
        if x < 0 then 1 - cd else cd


dsub1 : Float -> Float -> Float -> Float -> Float -> Float
dsub1 s x r t sigma =
    (logBase e (s / x) + (r + sigma^2 / 2) * t) / (sigma * sqrt t) 


dsub2 : Float -> Float -> Float -> Float -> Float -> Float
dsub2 s x r t sigma =
    (dsub1 s x r t sigma) - (sigma * sqrt t)


callPrice : Float -> Float -> Float -> Float -> Float -> Float
callPrice s x r t sigma =
    let
        cd1 = p (dsub1 s x r t sigma)
        cd2 = p (dsub2 s x r t sigma)
    in
        s * cd1 - x *  e^(-r * t) * cd2


putPrice : Float -> Float -> Float -> Float -> Float -> Float
putPrice s x r t sigma =
    let
        cd1 = p (negate (dsub1 s x r t sigma))
        cd2 = p (negate (dsub2 s x r t sigma))
    in
        x * e^(-r * t) * cd2 - s * cd1
