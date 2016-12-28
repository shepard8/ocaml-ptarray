open OUnit2
open PTArray

let l0 = []
let l1 = [0]
let l4 = [0; 1; 2; 3]
let l10 = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]

let a0 = [| |]
let a1 = [| 0 |]
let a10 = [| 0; 1; 2; 3; 4; 5; 6; 7; 8; 9 |]

let tests = [

  "length" >::: [
    "Empty" >:: (fun _ -> assert_equal 0 (length (of_list l0)));
    "Singleton" >:: (fun _ -> assert_equal 1 (length (of_list l1)));
    "10" >:: (fun _ -> assert_equal 10 (length (of_list l10)));
  ];

  "get" >::: [
    "Singleton" >:: (fun _ ->
      let a = of_list l1 in
      assert_equal 0 (get a 0);
    );
    "10" >:: (fun _ ->
      let a = of_list l10 in
      for i = 0 to 9 do
        assert_equal i (get a i)
      done
    );
    "Out of bounds" >:: (fun _ ->
      let a = of_list l10 in
      assert_raises Not_found (fun _ -> get a 11)
    );
  ];

  "set" >::: [
    "Simple" >:: (fun _ ->
      let a = of_list l10 in
      assert_equal 7 (get a 7);
      let b = set a 7 42 in
      for i = 0 to 9 do
        if i = 7 then assert_equal 42 (get b 7) else assert_equal i (get b i)
      done
    );
    "Out of bounds" >:: (fun _ ->
      let a = of_list l10 in
      assert_raises Not_found (fun _ -> set a 11 0)
    );
  ];

  "to_list" >::: [
    "Empty" >:: (fun _ -> assert_equal l0 (to_list (of_list l0)));
    "Singleton" >:: (fun _ -> assert_equal l1 (to_list (of_list l1)));
    "10" >:: (fun _ -> assert_equal l10 (to_list (of_list l10)));
  ];

  "to_array" >::: [
    "Empty" >:: (fun _ -> assert_equal a0 (to_array (of_array a0)));
    "Singleton" >:: (fun _ -> assert_equal a1 (to_array (of_array a1)));
    "10" >:: (fun _ -> assert_equal a10 (to_array (of_array a10)));
  ];

  "make" >::: [
    "Simple" >:: (fun _ ->
      let a = make 100 42 in
      for i = 0 to 99 do
        assert_equal 42 (get a i)
      done
    );
    "Negative" >:: (fun _ ->
      assert_equal 0 (length (make (-5) 42))
    );
  ];

  "init" >::: [
    "Empty" >:: (fun _ -> assert_equal (of_list l0) (init 0 (fun i -> i)));
    "Singleton" >:: (fun _ -> assert_equal (of_list l1) (init 1 (fun i -> i)));
    "10" >:: (fun _ -> assert_equal (of_list l10) (init 10 (fun i -> i)));
  ];

  "of_list" >::: [
    "Empty" >:: (fun _ -> ignore (of_list l0));
    "Singleton" >:: (fun _ -> ignore (of_list l1));
    "4" >:: (fun _ -> ignore (of_list l4));
    "10" >:: (fun _ -> ignore (of_list l10));
  ];

  "of_array" >::: [
    "Empty" >:: (fun _ -> assert_equal (of_list l0) (of_array a0));
    "Singleton" >:: (fun _ -> assert_equal (of_list l1) (of_array a1));
    "10" >:: (fun _ -> assert_equal (of_list l10) (of_array a10));
  ];
  
  "iter" >::: [
    "Sum" >:: (fun _->
      let a = of_list l10 in
      let sum = ref 0 in
      iter (fun x -> sum := !sum + x) a;
      assert_equal (!sum) 45
    );
    "Empty" >:: (fun _ ->
      let v = ref false in
      let a = of_list l0 in
      iter (fun _ -> v := true) a;
      assert_equal false (!v)
    );
  ];

  "iteri" >::: [
    "Sum" >:: (fun _->
      let a = of_list l10 in
      let sum = ref 0 in
      iteri (fun i x -> sum := !sum + i) a;
      assert_equal (!sum) 45
    );
    "Empty" >:: (fun _ ->
      let v = ref false in
      let a = of_list l0 in
      iteri (fun _ _ -> v := true) a;
      assert_equal false (!v)
    );
  ];

  "map" >::: [
    "Zeroify" >:: (fun _ ->
      let a = of_list l10 in
      let b = map (fun _ -> 0) a in
      assert_equal b (make 10 0)
    );
    "Boolify" >:: (fun _ -> 
      let a = of_list l10 in
      let b = map (fun x -> x < 5) a in
      for i = 0 to 4 do
        assert_equal true (get b i)
      done;
      for i = 5 to 9 do
        assert_equal false (get b i)
      done
    );
  ];

  "mapi" >::: [
    "Zeroify" >:: (fun _ ->
      let a = of_list l10 in
      let b = mapi (fun _ _ -> 0) a in
      assert_equal b (make 10 0)
    );
    "Boolify" >:: (fun _ -> 
      let a = of_list l10 in
      let b = mapi (fun i x -> i < 5) a in
      for i = 0 to 4 do
        assert_equal true (get b i)
      done;
      for i = 5 to 9 do
        assert_equal false (get b i)
      done
    );
  ];

  "fold_left" >::: [
    "Sum" >:: (fun _ ->
      let a = of_list l10 in
      let v = fold_left ( + ) 0 a in
      assert_equal 45 v
    );
    "Empty" >:: (fun _ ->
      let a = of_list l0 in
      let v = fold_left ( * ) 42 a in
      assert_equal 42 v
    );
  ];

  "foldi_left" >::: [
    "Sum" >:: (fun _ ->
      let a = of_list l10 in
      let v = fold_left (fun acc i v -> acc + i + v) 0 a in
      assert_equal 90 v
    );
    "Empty" >:: (fun _ ->
      let a = of_list l0 in
      let v = fold_left (fun acc i v -> acc * i * v) 42 a in
      assert_equal 42 v
    );
  ];








  "iter2" >::: [
    "Simple" >:: (fun _ ->
      let a = of_list l10 in
      let b = init 10 (fun i -> if i < 5 then 1 else 0) in
      let sum = ref 0 in
      iter2 (fun a b -> sum := !sum + a * b) a b;
      assert_equal 10 (!sum)
    );
  ];

  "map2" >::: [
    "Simple" >:: (fun _ ->
      let a = of_list l10 in
      let b = init 10 (fun i -> if i < 5 then 2 else 0) in
      let c = map2 ( * ) a b in
      assert_equal c (of_list [0; 2; 4; 6; 8; 0; 0; 0; 0; 0])
    );
  ];

  "fold_left2" >::: [
    "Sum of products" >:: (fun _ ->
      let a = of_list l10 in
      let b = init 10 (fun i -> if i < 5 then 1 else 0) in
      let v = fold_left2 (fun acc a b -> acc + a * b) 0 a b in
      assert_equal 10 v
    );
    "Non matching lengths" >:: (fun _ ->
      let a = of_list l10 in
      let b = init 11 (fun i -> i) in
      let v _ = fold_left2 (fun acc a b -> acc + a * b) 0 a b in
      assert_raises (Invalid_argument "List.fold_left2") v
    );
  ];




]

