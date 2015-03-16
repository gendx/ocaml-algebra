module Test = struct
  
  module Q = Common.Q
  module QMultiPoly = Common.QMultivarPoly
  
  let make_qmultipoly_of_int = QMultiPoly.make_of Q.of_int
  
  let test_spoly_impl x y z : bool =
    let px = make_qmultipoly_of_int x in
    let py = make_qmultipoly_of_int y in
    let pz = make_qmultipoly_of_int z in
    
    print_string "spoly(";
    QMultiPoly.print px;
    print_string ", ";
    QMultiPoly.print py;
    print_string ") = ";
    
    let s = (QMultiPoly.spoly px py) in
    QMultiPoly.print s;
    print_newline ();
    
    if QMultiPoly.is_zero (QMultiPoly.sub pz s) then
      true
    else (
      print_string "Unexpected result ! Expected was :";
      QMultiPoly.print pz;
      print_newline ();
      false
    )
  
  let test_spoly () =
    let _ = test_spoly_impl
      [1,[0,1 ; 1,2 ; 2,1] ; -1,[0,1 ; 1,1 ; 2,1]]
      [1,[0,2 ; 1,1 ; 2,1] ; -1,[2,2]]
      [-1,[0,2 ; 1,1 ; 2,1] ; 1,[1,1 ; 2,2]] in
    let _ = test_spoly_impl
      [1,[0,1 ; 1,1] ; 1,[2,3]]
      [1,[2,2] ; -3,[2,1]]
      [3,[0,1 ; 1,1 ; 2,1] ; 1,[2,5]] in
    let _ = test_spoly_impl
      [4,[0,2 ; 2,1] ; -7,[1,2]]
      [1,[0,1 ; 1,1 ; 2,2] ; 3,[0,1 ; 2,4]]
      [-12,[0,2 ; 2,4] ; -7,[1,3 ; 2,1]] in
    let _ = test_spoly_impl
      [1,[0,4 ; 1,1] ; -1,[2,2]]
      [3,[0,1 ; 2,2] ; -1,[1,1]]
      [1,[0,3 ; 1,2] ; -3,[2,4]] in
    ()
  
  let test_multipoly_div_impl px pf pr : bool =
    (*
    let px = make_qmultipoly_of_int x in
    let pf = Array.map make_qmultipoly_of_int f in
    let pr = make_qmultipoly_of_int rem in
    *)
    
    print_string "division(";
    QMultiPoly.print px;
    print_string ", {";
    Array.iter (fun t -> QMultiPoly.print t; print_string ", ") pf;
    print_string "}) = ";
    
    let q, r = QMultiPoly.division_set px pf in
    print_string "{";
    Array.iter (fun t -> QMultiPoly.print t; print_string ", ") q;
    print_string "} + ";
    QMultiPoly.print r;
    print_newline ();
    
    QMultiPoly.is_zero (QMultiPoly.sub pr r)
    
  let test_multipoly_div () =
    let g1 = make_qmultipoly_of_int [4,[0,2 ; 2,1] ; -7,[1,2]] in
    let g2 = make_qmultipoly_of_int [1,[0,1 ; 1,1 ; 2,2] ; 3,[0,1 ; 2,4]] in
    let s = QMultiPoly.spoly g1 g2 in
    let r = make_qmultipoly_of_int [] in
    let _ = test_multipoly_div_impl s (Array.of_list [g1 ; g2]) r in
    ()
    
  let test_groebner_impl x =
    let g = List.map make_qmultipoly_of_int x in
    let base = QMultiPoly.groebner g in
    print_endline "groebner of {";
    List.iter (fun t ->  print_string "\t"; QMultiPoly.print t; print_newline ()) g;
    print_endline "} = {";
    List.iter (fun t ->  print_string "\t"; QMultiPoly.print t; print_newline ()) base;
    print_string "}";
    print_newline ()
    
  let test_groebner () =
    test_groebner_impl [
      [4,[0,2 ; 2,1] ; -7,[1,2]] ;
      [1,[0,1 ; 1,1 ; 2,2] ; 3,[0,1 ; 2,4]] ;
    ];
    test_groebner_impl [
      [1,[0,2 ; 1,2 ; 2,2] ; -1,[]] ;
      [1,[0,2 ; 1,2 ; 2,2] ; 2,[0,1]] ;
      [2,[0,1] ; -3,[1,1] ; -1,[2,1]] ;
    ];
    test_groebner_impl [
      [1,[0,2] ; 1,[1,2] ; 1,[2,2] ; -1,[]] ;
      [1,[0,2] ; 1,[1,2] ; 1,[2,2] ; -2,[0,1]] ;
      [2,[0,1] ; -3,[1,1] ; -1,[2,1]]
    ];
    test_groebner_impl [
      [1,[0,1] ; 1,[1,1] ; 1,[2,1]] ;
      [1,[0,1] ; 1,[2,1]] ;
    ];
    test_groebner_impl [
      [1,[0,2 ; 1,1] ; 1,[0,1] ; 1,[]] ;
      [1,[0,1 ; 1,2] ; 1,[1,1] ; 1,[]] ;
      [1,[0,1] ; -1,[1,1]] ;
    ];
    test_groebner_impl [
      [1,[0,2 ; 1,1] ; -1,[2,3]] ;
      [2,[0,1 ; 1,1] ; -4,[2,1] ; -1,[]] ;
      [1,[2,1] ; -1,[1,2]] ;
      [1,[0,3] ; -4,[1,1 ; 2,1]] ;
    ];
    (*
    (* Hard one ! *)
    test_groebner_impl [
      [1,[3,2] ; 1,[0,2] ; 1,[1,2] ; 1,[2,2]] ;
      [1,[3,2] ; 2,[0,2] ; -1,[0,1 ; 1,1] ; -1,[2,2]] ;
      [1,[3,1] ; 1,[1,3] ; -1,[2,3]] ;
    ]
    *)
    
end
