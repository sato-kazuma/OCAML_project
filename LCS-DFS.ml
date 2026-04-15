(* ========================================================= *)
(* 
  PROGETTO LCS CON DFS
  INTRODUZIONE AI TIPI IN OCAML                            
  In OCaml ogni funzione ha un tipo del tipo:              
  tipo1 -> tipo2 -> ... -> risultato                     
  Questo NON è una tupla, ma una sequenza di parametri    
  (currying): ogni funzione prende un parametro alla volta 
                                                          
  Esempio:                                                 
  string -> string -> bool                               
  significa:                                               
  prende una stringa → restituisce una funzione che      
  prende un'altra stringa → restituisce un booleano      
  Il tipo unit equivale a void (nessun valore utile)        *)
(*========================================================= *)

(* =========================================================*)
(* LEGENDA                                            
 - fun x -> ... definisce una funzione anonima            
 - |> è una pipeline (passa il risultato avanti)          
 - :: aggiunge un elemento in testa alla lista            
 - | match e' l'equivalente del match case
 - ; sequenziamento delle istruzioni, una di seguito l'altra
 - ^ append per le stringhe
 - <> significa diverso *)
(* =========================================================*)

(* ===================================== *)
(* UTILITIES: funzioni di stampa         *)
(* ===================================== *)

(* Tipo: string list -> unit *)
let print_string_list_compact lst =
  (* Riceve in input una lista di stringhe e fa pattern match *)
  match lst with
  | [] -> print_endline "empty" (* Se è vuota, stampa empty *)
  | _ -> (* Altrimenti, stampa stringa s con uno spazio, per tutti gli elementi *)
      (* List.iter applica una funzione a ogni elemento *)
      List.iter (fun s -> print_string (s ^ " ")) lst;
      print_newline ()
;;

(* Tipo: string list -> unit *)
let print_lcs_grouped lcs_list =
  if lcs_list = [] then print_endline "empty"
  else
    (* La dimensione 10 è temporanea, è una stima dei risultati attesi. La hashtable può espandersi. *)
    let table = Hashtbl.create 10 in

    (* Inseriamo ogni stringa nella tabella per lunghezza *)

    (* List.iter Scorre ogni elemento della lista lcs_list *)
    List.iter (fun s -> (*Definiamo una funzione anonima *)
      (* Calcola la lunghezza della stringa s *)
      let len = String.length s in
      (* Controlla se esiste già in memoria con la chiave len *)
      if Hashtbl.mem table len then
        (* Se esiste la mette in testa alla lista esistente → :: *)
        Hashtbl.replace table len (s :: Hashtbl.find table len)
      else
        (* Altrimenti la aggiunge ad una nuova ista s *)
        Hashtbl.add table len [s]
    ) lcs_list;

    (* Estraiamo le chiavi (lunghezze) e le ordiniamo decrescenti *)
    let lengths =
      (* Scorre le coppie chiave valore, ottenendo una lista non ordinata *)
      Hashtbl.fold (fun k _ acc -> k :: acc) table []
      (* Ordina la lista in ordine decrescente *)
      |> List.sort (fun a b -> compare b a)
    in

    (* Stampiamo gruppo per gruppo *)
    List.iter (fun len ->
      (* Stampa la lunghezza K *)
      Printf.printf "Lunghezza %d: " len;
      (* Trova la lunghezza *)
      Hashtbl.find table len
      (* Ordina le stringhe e stmpale separandole con spazi *)
      |> List.sort compare
      |> print_string_list_compact
    ) lengths
;;

(* ===================================== *)
(* CHECK SOTTOSEQUENZE                   *)
(* ===================================== *)

(* Tipo: string -> string -> bool *)
let is_subsequence sub str =
  (* Funzione ricorsiva interna *)
  let rec aux i j =
    (* 
      Se abbiamo scansionato tutti i caratteri della stringa sub 
      e abbiamo trovato tutti i caratteri nell'ordine corretto in str
      ritorna vero, altrimenti sub non può essere sottosequenza di str e quindi
      torna false
    *)
    if i = String.length sub then true
    else if j = String.length str then false
      (* se il carattere corrisponde, cambiamo carattere ad entrambe le stringhe + 1 *)
    else if sub.[i] = str.[j] then aux (i+1) (j+1)
      (* altrimenti dobbiamo cercare il carattere successivo di str, non di sub *)
    else aux i (j+1)
  in aux 0 0
;;

(* Tipo: string -> string list -> bool *)
let is_common_subsequence candidate string_list =
  (* List.for_all verifica che la condizione sia vera per tutti *)
  List.for_all (fun s -> is_subsequence candidate s) string_list
;;

(* ===================================== *)
(* DFS - CUORE DELL'ALGORITMO            *)
(* ===================================== *)

(* Tipo:
   string -> string list -> int -> string -> string list -> string list
   shortest = stringa base
   others = altre stringhe
   index = posizione corrente
   current = sottosequenza costruita
   acc = accumulatore risultati
*)

let rec dfs_lcs shortest others index current acc =
  if index = String.length shortest then
    (* Caso base: stringa finita *)
    if is_common_subsequence current others then current :: acc
    else acc
  else
    (* Costruisco il carattere corrente *)
    let c = String.make 1 shortest.[index] in

    (* RAMO 1: includo il carattere *)
    let acc_with = dfs_lcs shortest others (index+1) (current ^ c) acc in

    (* RAMO 2: non includo il carattere *)
    dfs_lcs shortest others (index+1) current acc_with
;;

(* ===================================== *)
(* FUNZIONI PRINCIPALI                   *)
(* ===================================== *)

(* Tipo: string list -> string list *)
let find_all_lcs string_list =
  match string_list with
  | [] | [""] -> [""] (* Se non troviamo stringhe o stringa vuota, , non esiste sottosequenza e tirnorno stringa vuota *)
  | [_] -> string_list (* Se c'è una sola stringa, allora ogni sottosequenza è valida, quindi restituisco la lista**)
  | _ ->
      (* Ordino le stringhe per lunghezza *)
      let sorted =
        List.sort (fun a b -> compare (String.length a) (String.length b)) string_list
      in

      (* PRendiamo la stringa più corta, che si trova in cima *)
      let shortest = List.hd sorted in
      let others = List.tl sorted in

      (* Avvio la DFS , ottenendo tutte le sottosequenze comuni generate *)
      let all = dfs_lcs shortest others 0 "" [] in

      (* Rimuovo duplicati *)
      List.sort_uniq compare all
;;

(* Tipo: string list -> int -> string list *)
let find_lcs_ge_k string_list k =
  (* Trova tutte le lcs comuni *)
  let all = find_all_lcs string_list in

  (* Filtro per lunghezza >= k *)
  let filtered = List.filter (fun s -> String.length s >= k) all in

  (* Ordino per lunghezza e poi alfabetico, decrescente → infatti inverti a b *)
  List.sort (fun a b ->
      let len_cmp = compare (String.length b) (String.length a) in
      (* Se le lunghezze sini uguali, ordina alfabeticamente, altrimenti per lunghezza *)
      if len_cmp = 0 then compare a b else len_cmp
    ) filtered
;;

(* ===================================== *)
(* INTERAZIONE UTENTE                    *)
(* ===================================== *)

(* Tipo: string list -> unit *)
let ask_for_k_and_check string_list =
  print_endline "\nInserisci un valore k (lunghezza minima desiderata):";
  try
    (* Definisci k un intero, che prendi dall'utente *)
    let k = read_int () in
    (* Lancia la funzione find_lcs_ge_k con questo k *)
    let lcs_k = find_lcs_ge_k string_list k in

    if lcs_k = [] then
      (* Se il risultato è vuoto *)
      print_endline ("\nNon esiste alcuna sottosequenza comune di lunghezza >= " ^ string_of_int k)
    else (
      (* Stampa quello che hai trovato *)
      print_endline ("\nSottosequenze di lunghezza >= " ^ string_of_int k ^ ":\n");
      print_lcs_grouped lcs_k
    )
    (* Se il controllo su k da errore *)
  with Failure _ -> print_endline "Inserisci un numero intero valido!"
;;

(* Tipo: unit -> unit *)
let rec main_menu () =
  (* Stampa il menù per l'utente *)
  print_endline "\n=== MENU ===";
  print_endline "1) Inserisci lista di stringhe e valore k";
  print_endline "2) Esci\n";
  print_string "Scelta: ";

  match read_line () with
  | "1" ->
    (* L'utente deve inserire stringhe, senza controlli di dimensione o input *)
      print_endline "\nInserisci le stringhe separate da spazi:";
      let input = read_line () in

      (* Split della stringa in lista *)
      let strings =
        (* Divide la stringa basandosi sugli spazi *)
        String.split_on_char ' ' input
        (* le filtriamo eliminando stringhe vuote *)
        |> List.filter (fun s -> s <> "")
      in

      (* Da qui partono le funzioni: da notare che essendo rec, 
      la richiesta non termina a meno che l'utente non lo decida *)
      ask_for_k_and_check strings;
      main_menu ()

  | "2" -> print_endline "\nUscita..." (* Diamo la possibilità all'utente di terminare il programma *)
  | _ -> print_endline "\nOpzione non valida."; main_menu () (* Se l'utente non sceglie queste due opzioni, il programma ritenta *)
;;

(* ===================================== *)
(* TEST AUTOMATICI                       *)
(* ===================================== *)

(* Tipo: string -> string list -> int -> unit *)

(* Test manager e runn all test per dare in pasto al programma delle stringhe e dei k di prova *)

let run_test name string_list k =
  Printf.printf "\n===== %s =====\n" name;
  print_string "Input: ";
  print_string_list_compact string_list;
  Printf.printf "Valore k = %d\n" k;

  let lcs_k = find_lcs_ge_k string_list k in

  Printf.printf "Sottosequenze ≥ k:\n";
  print_lcs_grouped lcs_k
;;

(* Tipo: unit -> unit *)
let run_all_tests () =
  run_test "Classic LCS" ["abcde"; "ace"; "aebdc"] 2;
  run_test "No common subsequence" ["abc"; "def"; "ghi"] 1;
  run_test "Identical strings" ["mario"; "mario"; "mario"] 3;
  run_test "Non-contiguous subsequence" ["abcdef"; "azced"; "ace"] 2;
  run_test "Single string" ["abc"] 1;
  run_test "Empty string in list" ["abc"; ""; "abc"] 1;
  run_test "Multiple LCS" ["abcbdab"; "bdcaba"; "badacb"] 3;
  run_test "Special characters" ["a!b@c"; "a!x@c"; "!a@c"] 2;
  run_test "Repeated characters" ["aaabbb"; "ababab"; "aabb"] 2;
  run_test "Very long strings" ["abcdefghijklmnop"; "acegikmo"; "afkp"] 4
;;

(* ===================================== *)
(* MAIN - PUNTO DI INGRESSO              *)
(* ===================================== *)

(* Flusso:
   main
   → run_all_tests
   → main_menu
   → ask_for_k_and_check
   → find_lcs_ge_k
   → find_all_lcs
   → dfs_lcs
   → is_common_subsequence
   → is_subsequence
*)

let () =
  print_endline "\nInizio programma LCS interattivo con test automatici migliorati!";
  run_all_tests ();
  main_menu ()
;;

(* #use "nomeprogramma.ml";; in utop*)
(* Sys.command "clear";; per pulire tutto *)
