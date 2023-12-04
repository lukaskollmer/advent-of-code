theory Day04
  imports Main "../Auxiliary"
begin



term nth
term split_list




fun parse_numbers_imp :: "string \<Rightarrow> nat set" where
  "parse_numbers_imp input =
    split_list input (CHR '' '')
    |> filter (\<lambda>s. length s > 0)
    |> map nat_of_string
    |> set
  "



fun parse_numbers :: "string \<Rightarrow> (nat set \<times> nat set)" where
  "parse_numbers input = (
    let input = nth (split_list input (CHR '':'')) 1 in
    let (winning, mine) = (let split = split_list input (CHR ''|'') in (split!0, split!1))in
    (parse_numbers_imp winning, parse_numbers_imp mine)
  )"



fun calc_points :: "nat \<Rightarrow> nat" where
  "calc_points 0 = 0"
| "calc_points (Suc 0) = 1"
| "calc_points (Suc n) = 2 * calc_points n"


fun pt01 :: "string list \<Rightarrow> nat" where
  "pt01 input =
    input
    |> map parse_numbers
    |> map (\<lambda> (winning, mine). card (winning \<inter> mine))
    |> map calc_points
    |> foldl (+) 0
  "






definition sample_input :: "string" where
  "sample_input =
''Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11''"


value "pt01 (split_lines sample_input)"

lemma "pt01 (split_lines sample_input) = 13"
  by eval

export_code pt01 in OCaml


end