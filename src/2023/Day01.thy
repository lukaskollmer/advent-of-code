theory Day01
  imports Main Auxiliary
begin




fun first_where :: "'a list \<Rightarrow> ('a \<Rightarrow> bool) \<Rightarrow> 'a option" where
  "first_where [] _ = None"
| "first_where (x#xs) p = (if p x then Some x else first_where xs p)"


value "(of_char (CHR ''0'')::nat)"
value "(of_char (CHR ''0'')::nat) < (of_char (CHR ''1'')::nat)"


fun opt_get :: "'a option \<Rightarrow> 'a" where
  "opt_get (Some x) = x"


fun pt01 :: "string list \<Rightarrow> nat" where
  "pt01 input =
    foldl (+) 0 (map (\<lambda>x. let a = opt_get (first_where x is_ascii_char);
                              b = opt_get (first_where (rev x) is_ascii_char)
                          in (nat_of_char' a) * 10 + (nat_of_char' b)) input)"


fun pt02 :: "string list \<Rightarrow> nat" where
  "pt02 xs = 0"


fun run :: "string list \<Rightarrow> (nat \<times> nat)" where
  "run xs = (pt01 xs, pt02 xs)"


(* export_code run in OCaml *)

end