(* 
* File: efolioA.ml 
*
* UC: Linguagens de Programação
*
* by Diogo Antão
* 03/04/2021
*)

open Printf

(* definição de uma enum para o tipo de dados "risco" *)
type risco = Alto | Medio | Baixo | Desconhecido | Inconclusivo

(* definição de um record para o tipo de dados "senior" *)
type senior = {
  nutente : int;
  ndoencas : int;
  nmedicamentos : int;
  acidente : bool;
  doenca : bool;
  sozinho : bool;
  autonomia : bool;
  desportoj : bool;
  autofisica : bool;
  fisica : bool;
  profissaorisco : risco
}

(* definição do número de factos para cada senior *)
let n_factos = 11

(* definição do ficheiro de input *)
let file = "data/factos.txt"



(* função para leitura de dados a partir de um ficheiro; é criada uma lista de strings *)
let read_lines filename =
  if Sys.file_exists filename then
    begin
      let in_ch = open_in filename in
      let try_read () =
        try Some (input_line in_ch) with End_of_file -> None in
      let rec loop lines_list = match try_read () with
        | Some str -> loop (str :: lines_list)
        | None -> close_in in_ch; List.rev lines_list in
      loop []
    end
  else [];;


(* função que converte uma string num bool *)
let my_bool_of_string str = match str with
  | "s" -> true
  | "S" -> true
  | "n" -> false
  | "N" -> false
  | _   -> false;;


(* função que converte uma string num tipo de risco *)
let risco_of_string str = match str with
  | "alto"  -> Alto
  | "medio" -> Medio
  | "baixo" -> Baixo
  | _       -> Desconhecido;;


(* função que converte um tipo de risco numa string *)
let string_of_risco r = match r with
  | Alto  -> "Alto"
  | Medio -> "Medio"
  | Baixo -> "Baixo"
  | _     -> "Inconclusivo";;


(* função que cria uma lista de listas de strings a partir de uma lista de strings *)
let create_list_list str_l =
  (* A função split cria uma lista de strings a partir de uma string "cortando" esta última sempre que encontra o carater ';' *)
  let split line = String.split_on_char ';' line in
  List.map split str_l;;


(* função que filtra uma lista de listas consoante a dimensao de cada lista interior *)
let filter_list_by_dim ll = 
  let verifica_n_campos lista_interior = if List.length lista_interior = n_factos then true else false in
  List.filter verifica_n_campos ll;;


(* função que cria uma lista de seniores a partir de uma lista de listas de strings *)
let create_senior_list ll =
  (* A função senior_of_list converte uma lista de strings num senior *)
  let senior_of_list l = {
    nutente = int_of_string (List.nth l 0);
    ndoencas = int_of_string (List.nth l 1);
    nmedicamentos = int_of_string (List.nth l 2);
    acidente = my_bool_of_string (List.nth l 3);
    doenca = my_bool_of_string (List.nth l 4);
    sozinho = my_bool_of_string (List.nth l 5);
    autonomia = my_bool_of_string (List.nth l 6);
    desportoj = my_bool_of_string (List.nth l 7);
    autofisica = my_bool_of_string (List.nth l 8);
    fisica = my_bool_of_string (List.nth l 9);
    profissaorisco = risco_of_string (List.nth l 10)
  }
  in
  List.map senior_of_list ll;;


(* função que cria uma lista de riscos a partir de uma lista de seniores *)
(* a lista de riscos tem exatamente a mesma dimensão e ordem da lista de seniores *)
let create_risco_list sl = 
  (* A função decide_risco decide qual o grau de risco correspondente a cada senior *)
  let decide_risco s = match s with
    | {
      ndoencas;
      nmedicamentos;
      doenca = true;
      autonomia = false;
      autofisica = false;
      profissaorisco
    } when ndoencas > 2 && nmedicamentos > 3 && (profissaorisco = Alto || profissaorisco = Medio) -> Alto
    | {
      ndoencas;
      nmedicamentos;
      desportoj = false;
      fisica = false;
      profissaorisco
    } when ndoencas <= 2 && nmedicamentos <= 3 && (profissaorisco = Medio || profissaorisco = Baixo) -> Medio
    | {
      nmedicamentos;
      acidente = false;
      doenca = false;
      sozinho = false;
      profissaorisco = Baixo
    } when nmedicamentos <= 2 -> Baixo
    | _ -> Inconclusivo
  in
  List.map decide_risco sl;;


(* funções para impressão de uma tabela na consola *)
let print_aster () = printf "**********************************\n";;
let print_header () = printf "*    Utente     *      Risco     *\n";;


(* função que toma como argumentos um senior e um risco a ele associado e imprime na consola o número de utente do senior e qual o respetivo grau de risco *)
let print_risco s r = printf "*  %10d   *  %12s  *\n" s.nutente (string_of_risco r);;


(* função que itera conjuntamente a lista de seniores e a lista de riscos associada e chama a função print_risco a cada iteração *)
let print_all rl sl =
  let f s r = print_risco r s in
  List.iter2 f rl sl;;



(* função principal que executa de facto o programa *)
let main =
  let m_string_list = read_lines file in
  let m_list_list = create_list_list m_string_list in
  let m_filtered_list = filter_list_by_dim m_list_list in
  let m_senior_list = create_senior_list m_filtered_list in
  let m_risco_list = create_risco_list m_senior_list in
    begin
      print_aster ();
      print_header ();
      print_aster ();
      print_all m_risco_list m_senior_list;
      print_aster ()
    end;;