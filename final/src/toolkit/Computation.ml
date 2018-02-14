
(* Calculus gives an abstraction over computations so we can have a model
   of calculus adapted to play with the flow of execution.
*)
module Calculus = struct

  (* A calculus of type ['a t] is defined by a sequence of operations which
     will eventually yield a result of type 'a.
     It is conceptually similar to a 'Promess' or a 'Future'.
     
     [Finished result] describes a finished calculus holding a result.

     [Callback (x, f)] is a composition of the calculus x and its
     continuation represented by f which should in turn return a calculus.

     [Frozen x] is a frozen calculus that cannot advance in its computation
     until it is unfrozen by some exterior mean.
  *)
  type 'a t =
  | Finished : 'a -> 'a t
  | Callback : 'a t * ('a -> 'b t) -> 'b t
  | Frozen   : 'a t -> 'a t

  (* [return x] simply builds a calculus holding a result x. *)
  let return x = Finished x

  (* [bind x f] returns the calculus x with the continuation f.
     Here we try to keep the "head" of the calculus at the top of
     the structure to avoid needlessly going up and down the tree during
     evaluation.
     <<Not really sure it actually does what we want. To investigate.>>
  *)
  let rec bind x f = match x with
    | Frozen x -> Frozen (Callback (x, f))
    | Callback (m, g) -> Callback (m, fun r -> bind (g r) f)
    | Finished _ -> Callback (x, f)

  (* [run x] runs the calculus x until its end, unfreezing it along the way
     to progress. *)
  let run =
    let rec aux : type a b. (a -> b) -> a t -> b = fun k -> function
      | Callback (x, f) -> aux (fun r -> aux k (f r)) x
      | Finished x -> k x
      | Frozen x -> aux k x
    in fun m -> aux (fun x -> x) m

  (* [step x] takes one step in the calculus x and returns the resulting
     calculus. *)
  let rec step : type a. a t -> a t = function
    | Callback (Finished x, f) -> f x
    | Callback (x, f) -> bind (step x) f
    | Finished x -> Finished x
    | Frozen _ as fr -> fr

  (* [nstep n x] takes n step in the calculus x when possible and returns
     the resulting calculus.
     Given m the number of steps necessary for the calculus to hold a result or
     be frozen, if n > m then nstep makes m steps and returns the resulting
     calculus silently.
  *)
  let rec nstep n c =
    if n <= 0 then c else match c with
    | Frozen _
    | Finished _ -> c
    | _ -> nstep (n-1) (step c)

  (* [freeze x] returns the frozen version of the calculus x which cannot make
     any further progress in its computation.
     If x is already frozen, returns x silently.
  *)
  let freeze = function
    | Frozen _ as m -> m
    | m -> Frozen m

  (* [defreeze x] returns the calculus x without the frozen state so it can now
     make progress.
  *)
  let defreeze = function
    | Frozen m -> m
    | m -> m

  (* some infix and prefix operators to ease the use of some functions. *)
  module Operators = struct
    let ( >>= ) = bind
    let ret = return
    let ( !! ) = freeze
    let ( ?? ) = defreeze
  end

end

(* Execution permits/tries to run concurrent executions of the above calculi
   with some additional features such as controlling the priority of execution
   of the concurrent computations.
*)
module Execution = struct

  (* We will need the Calculus module to achieve our goal.
     Might be renamed at some point because cool names are
     what matters the most. ^^
  *)
  open Calculus

  (* We adopt something similar to a pid system with the idea that we
     want to ensure an execution can provide a unique way to identify a
     registered computation in its context.
     We will have to force this property somehow by hiding critical functions.
 
    <<We could make it so that the module Id provides only fresh ids.
     This could be useful if we want to move computations from one execution to
     another easily without worrying about ids collision and be able to identify
     uniquely a computation in any execution rather than in a particular one.
     >>
  *)
  module Id = struct
    type t = int
    let compare x y = x - y
  end

  (* We need to be able to store calculi and to retrieve them when needed.
     We would like to also have the capacity to remove some computation from the
     execution, move it around or do any crazy thing without exploding the
     complexity meter.

     <<For now we stick with a Map structure to store computations, it might
     not be the best but it is a straight forward way to implement what we need.
     It is exposed in the signature. Maybe we want to hide that?
     >>
  *)
  module IdMap = Map.Make (Id)

  (* One of our goal is to be able to modify the priority of our computations
     in a 'concurrent' context.
     Basic int should suffice to hold this information.
  *)
  type priority = int

  (* an alias for the identity of computations. *)
  type id = Id.t

  (* We now define what is a process in our context.
     It is the combination of 3 fields :

     [id] : a unique identity in its execution context.

     [priority] : its priority of execution.

     [computation] : the actual calculus.
  *)
  type 'a process = {
    id : id;
    priority : priority;
    computation : 'a Calculus.t;
  }

  (* Helper function to make a process *)
  let mk id priority computation = {
    id; priority; computation;
  }

  (* We define what is an execution.

     [nextpid]  : An execution contains its own identity system so we need to
                  keep track of this somehow.

     [running]  : currently running processes.

     [finished] : finished computations holding a result.

     [frozen]   : frozen calculi
     (Here freezing acts as some sort of barrier in a concurrent execution.
     Every frozen computation will be put on hold until there are no more
     running processes, then all the frozen ones will be restored as running
     processes to continue the execution.)
  *)
  type 'a execution = {
    nextpid : int;
    running : 'a process IdMap.t;
    finished : 'a process IdMap.t;
    frozen   : 'a process IdMap.t;
  }

  (* An intial empty execution to build upon. *)
  let empty = {
    nextpid = 0;
    running = IdMap.empty;
    finished = IdMap.empty;
    frozen = IdMap.empty;
  }

  (* [_add id priority calculus execution] returns the execution resulting from
     adding the process with the given id priority and calculus.
     It stores it in the appropriate Map based on the state of the calculus.

     Warning : this can break the uniqueness of the ids in the execution if
     not used with a proper id.
  *)
  let _add i p c e =
    let pc = mk i p c in
    match c with
    | Finished _ -> {e with finished = IdMap.add i pc e.finished}
    | Frozen _ -> {e with frozen = IdMap.add i pc e.frozen}
    | Callback _ -> {e with running = IdMap.add i pc e.running}

  (* [add priority calculus e] returns e with the added process described by
     priority and calculus.
     It gives the created process a proper unique id in the resulting execution.
  *)
  let add ?(priority = 1) c e =
    let pid = e.nextpid in
    let e = {e with nextpid = e.nextpid + 1} in
    _add pid priority c e, pid

  (* [extract_process id e] return the process associated with the given id in execution e and
     the execution e where the process has been removed.
     raises Not_found if no process p could be found for the given id in e.
  *)
  let extract_process =
    let ( >>= ) x y = try x () with _ -> y () in
    let extract_process id m =
      let p = IdMap.find id m in
      let m = IdMap.remove id m in
      p, m
    in fun id e ->
      (fun () ->
	let p, running = extract_process id e.running in
	p, {e with running}) >>= fun () -> ((fun () ->
	  let p, finished = extract_process id e.finished in
	  p, {e with finished}) >>= (fun () ->
	    let p, frozen = extract_process id e.frozen in
	    p, {e with frozen}))

    (* [add_process priority pc e] returns the execution e with the added process pc at the
       specified priority and its id in this execution.
    *)
    let add_process ?(priority = 1) pc e =
      add ~priority pc.computation e
			  
  (* [exec_defreeze e] retrieves all the frozen computations of the
     execution e and restore them as running processes.

     Warning : If there are still running processes in e they will be erased in
     the resulting execution.
  *)
  let exec_defreeze e =
    let running = IdMap.map (fun pc ->
      {pc with computation = Calculus.defreeze pc.computation}
    ) e.frozen in
    {e with running; frozen = IdMap.empty}

  (* [exec_step e] returns the execution resulting from advancing one step in e.
     It implements priority by taking n steps in a process p when
     its priority is n.

     <<Not sure if it is how we want this to work because one process might
     execute itself for an arbitrary long time before letting others work.
     Reasonning based on relative priorities of processes rather than absolute
     ones would surely yield better results.
     For now it is easier to implement and 'should' not do awkward stuff
     if priorities are not out of the charts.
     >>
  *)
  let exec_step e =
    let exec _ pc e =
      let c = Calculus.nstep pc.priority pc.computation in
      let e = _add pc.id pc.priority c e in
      e
    in
    let e = IdMap.fold exec e.running {e with running = IdMap.empty} in
    if IdMap.is_empty e.running && not (IdMap.is_empty e.frozen) then
      exec_defreeze e
    else e

  (* [exec_run e] tries to run completely the execution e, that is it executes
     it until there are no more running processes.
     This might never happen in presence of non terminating computations including
     infinitely often freezing processes going back and forth from running to
     frozen state.
  *)
  let rec exec_run e =
    if IdMap.is_empty e.running then e
    else exec_run (exec_step e)

  (* [results e] returns the current results hold by the finished computations
     of the execution e.
     It relies on the fact that we only add finished calculi in the finished
     store so we can safely call run to retrieve the result without risking
     looping.
  *)
  let results e =
    IdMap.map (fun pc ->
      Calculus.run pc.computation
    ) e.finished


  (* [frozen e] returns the map of the currently frozen computations in the
     execution e.
  *)
  let frozen e =
    IdMap.map (fun pc -> pc.computation) e.frozen

  (* [running e] behaves like the above function but for running computations.
  *)
  let running e =
    IdMap.map (fun pc -> pc.computation) e.running

  (* [change_priority id p m] return the map m where the process identified
     by id has the new priority p.
     
     raises Not_found if there is no process for the given id in m.
  *)
  let change_priority id p m =
    let pc = IdMap.find id m in
    let pc = {pc with priority = p} in
    IdMap.add id pc m

(* [modify_priority id p e] attempts to modify the priority of the
   process identified by id in the execution e.
   
   It is first searched among the running processes and if this fails it is
   then searched among the frozen ones.

   raises Not_found if either the process is finished in e
   or does not belong to e

   <<Not sure if changing the priority of a finished computation will ever
   be needed.
   Maybe the client knows where we can find the id he is referring to?>>
*)
  let modify_priority id p e =
    try
      {e with running = change_priority id p e.running}
    with Not_found ->
      {e with frozen = change_priority id p e.frozen}

end




(* Sample of code using this.
   Monothreaded concurrent execution of several factorials computations
   with some side effects to see what is happening.

   <<Hard to tell the real cost of this implementation with a 'toy' like this.
    We need something HARDCORE.
   >>
*)


open Calculus.Operators
open Execution


let testfact () =

  let fact whoami n =
  (* we will count the recursive calls to fact.*)
    let step = ref 0 in
    let rec fact r n =
      ret n >>= fun n ->
    (* printing to see something happen *)
      Printf.printf "%s r:%d step:%d\n%!" whoami r (!step);
      incr step;
    (* usual return condition *)
      if n = 0 then begin
	Printf.printf "%s has finished\n%!" whoami;
	ret r
      end
    (* or we continue *)
      else ret (r * n) >>= fun r ->
      ret (n-1) >>= fact r
    in
  (* inital call *)
    fact 1 n
  in

  (* Note that a single step of execution is assumed to be enclosed between
     two binds.
     Meaning that to ensure atomicity, the atomic part should be written
     without binds.
     
     Anything of the form x >>= fun x -> ... is fair game for concurrency
     based on execution priorities and one should not assume priorites when
     describing computations.

     Here we wrote fact with a lot of binds, slowing down the computation in
     our implementation. A better 'fact' in our case is one with only one bind
     to create a point in the execution allowing concurrency to happen without
     paying to much.

     let fact2 n =
       let rec fact r n =
         ret n >>= fun n ->
         if n = 0 then
	   ret r
         else fact (r * n) (n - 1)
       in
       fact 1 n
  *)

  (* Creating the execution *)
  let e, id1 = add (fact "[1]" 10) empty in
  let e, id2 = add ~priority:2 (fact "[2]" 10) e in
  let e, id3 = add ~priority:3 (fact "[3]" 10) e in
  (* Running it step by step *)
  let e = exec_step e in
  let e = exec_step e in
  print_endline "extracting 2";
  let pc, e = extract_process id2 e in
  let e = exec_step e in
  let e = exec_step e in
  let e = exec_step e in
  let e = exec_step e in
  print_endline "reinjecting 2";
  let e, _ = add_process pc e in
  print_endline "modifying priority of 1";
  (* We modify the priority setting of the process with id1 in e *)
  let e = modify_priority id1 7 e in
  (* Run it until it ends and prints the results *)
  let e = exec_run e in
  IdMap.iter (fun id r -> Printf.printf "%d %d\n" id r) (results e)

(* Some fun with shared memory represented as a reference to work cooperatively
   on a factorial computation.
*)
let cooperation () =

  let unit = ret () in

  let fact n =
    let state = ref (1, n) in
    let rec aux who = unit >>= fun () ->
      let r, n = !state in
      Printf.printf "%s %d %d\n" who r n;
      if n = 0 then ret r
      else (
	state := (r * n, n - 1);
	aux who
      )
    in aux
  in

  let coopfact = fact 20 in
  let ex, _ = add (coopfact "0") empty in
  let ex, _ = add (coopfact "1") ex in
  let ex, _ = add (coopfact "2") ex in
  let results = results (exec_run ex) in
  IdMap.iter (fun id r -> Printf.printf "%d %d\n" id r) results

