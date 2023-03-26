(* Test Supplémentaire*)

let abs_property x = x >= 0.;;
let abs_property_bis x = x<0.;;
let abs_test = Test.make_test
    (Generator.float (-20.) 20.)
    (Reduction.float)
    (abs_property);;
let abs_test_bis = Test.make_test
    (Generator.float (-20.) 20.)
    (Reduction.float)
    (abs_property_bis);;
let result = Test.check 1000 abs_test;;
let fail= Test.fails_at 1000 abs_test;;
let execute = Test.execute 1000 [abs_test; abs_test_bis];;
Printf.printf "Test result: %b\n" result;;