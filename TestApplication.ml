#use "Test.ml" ;;

(*Test de la fonctionnalité supplémentaire combine du module Test*)

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
let report = Test.generate_report 1000 test_list;;
print_string report;;
