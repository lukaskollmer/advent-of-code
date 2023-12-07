theory Day06
  imports Main "../Auxiliary"
begin





section \<open>Utils\<close>


fun mk_range_to_excl :: "nat \<Rightarrow> nat list" where
  "mk_range_to_excl 0 = []"
| "mk_range_to_excl (Suc n) = (mk_range_to_excl n) @ [n]"




section \<open>Pt01\<close>

type_synonym race = "nat \<times> nat"



fun parse_input_aux :: "nat list \<Rightarrow> nat list \<Rightarrow> race list" where
  "parse_input_aux [] [] = []"
| "parse_input_aux (t#ts) (d#ds) = (t, d) # (parse_input_aux ts ds)"


fun parse_input :: "string list \<Rightarrow> race list" where
 "parse_input input = (
    let input = input
      |> map (split_list (CHR '' ''))
      |> map (filter (\<lambda>l. length l \<noteq> 0))
      |> map tl
      |> map (map nat_of_string)
    in parse_input_aux (input!0) (input!1))
  "



fun calc_distances :: "nat \<Rightarrow> nat list" where
  "calc_distances n = mk_range_to_excl n |> map (\<lambda>n'. (n-n')*n')"


value "calc_distances 7"


fun pt01 :: "string list \<Rightarrow> nat" where
  "pt01 input =
    input
      |> parse_input
      |> map (\<lambda>(t, r). calc_distances t |> count_where (\<lambda>d. d > r))
      |> foldl (*) 1
  "




definition input where
  "input =
''Time:        57     72     69     92
Distance:   291   1172   1176   2026''"



value "split_lines input
  |> map (split_list (CHR '' ''))
  |> map (filter (\<lambda>l. length l \<noteq> 0))
  |> map tl
  |> map (map nat_of_string)
"




(* value "input |> split_lines |> pt01" *)

lemma "input |> split_lines |> pt01 = 160816"
  (* for some reason writing it like this (having the lemma verify that the result is correct,
     and using the `eval` method) works acceptably quick, while using the `value` command above
     would take forever...
  *)
  by eval





section \<open>Pt02\<close>


fun parse_input' :: "string list \<Rightarrow> race" where
 "parse_input' i = (
    let input = i
      |> map (split_list (CHR '' ''))
      |> map (filter (\<lambda>l. length l \<noteq> 0))
      |> map tl
      |> map (concat)
      |> map nat_of_string
    in (input!0, input!1))
  "



(* fun pt02 :: "string list \<Rightarrow> nat" *)
  

end