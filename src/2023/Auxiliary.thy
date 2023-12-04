theory Auxiliary
  imports Main
begin



fun id :: "'a \<Rightarrow> 'a" where
  "id x = x"



definition pipe :: "'a \<Rightarrow> ('a \<Rightarrow> 'b) \<Rightarrow> 'b"  (infixl "|>" 55)
  where "x |> f = f x"

notation (ASCII)
  pipe  (infixl "|>" 55)



fun split_list_aux :: "'a list \<Rightarrow> 'a \<Rightarrow> 'a list list \<Rightarrow> 'a list \<Rightarrow> 'a list list" where
  "split_list_aux [] _ acc acc' = acc @ [acc']"
| "split_list_aux (x#xs) a acc acc' = (
    if x = a then split_list_aux xs a (acc @ [acc']) []
    else split_list_aux xs a acc (acc' @ [x])
  )"



fun split_list :: "'a list \<Rightarrow> 'a \<Rightarrow> 'a list list" where
  "split_list [] _ = []"
| "split_list xs a = split_list_aux xs a [] []"

(*
fun split_list' :: "'a list \<Rightarrow> 'a \<Rightarrow> 'a list list" where
  "split_list' [] _ = []"
| "split_list' (x#xs) a = (
    if x = a then undefined
    else 
  )"*)




value "split_list ''Hello World'' (CHR '' '')"
value "split_list ''a'' (CHR ''a'') "



fun list_join :: "'a list list \<Rightarrow> 'a \<Rightarrow> 'a list" where
  "list_join [] _ = []"
| "list_join [[]] j = [j]"
| "list_join (x#xs) j = (x @ [j]) @ (list_join xs j)"



lemma "\<lbrakk> s = split_list xs a \<rbrakk> \<Longrightarrow> list_join s a = xs"
  oops




fun count_in_list :: "'a list \<Rightarrow> 'a \<Rightarrow> nat" where
  "count_in_list [] _ = 0"
| "count_in_list (x#xs) a = Suc (count_in_list xs a)"


(* lemma "xs \<noteq> [] \<Longrightarrow> length (split_list xs a) = Suc (count_in_list xs a)" *)






(*fun split_list' :: "'a list \<Rightarrow> 'a \<Rightarrow> 'a list list" where
  "split_list' []     _ = []"
| "split_list' (x#xs) s = (if x = s then )"*)





definition nat_of_char :: "char \<Rightarrow> nat" where
  "nat_of_char = of_char"

definition char_of_nat :: "nat \<Rightarrow> char" where
  "char_of_nat = char_of"



definition nat_of_char' :: "char \<Rightarrow> nat" where
  "nat_of_char' c = (nat_of_char c) - (nat_of_char (CHR ''0''))"


fun is_ascii_char :: "char \<Rightarrow> bool" where
  "is_ascii_char c = (nat_of_char (CHR ''0'') \<le> nat_of_char c \<and> nat_of_char c \<le> nat_of_char (CHR ''9''))"



fun trim_list_front :: "'a list \<Rightarrow> 'a \<Rightarrow> 'a list" where
  "trim_list_front [] _ = []"
| "trim_list_front (x#xs) a = (if x = a then trim_list_front xs a else (x#xs))"


fun trim_list :: "'a list \<Rightarrow> 'a \<Rightarrow> 'a list" where
  "trim_list xs a = trim_list_front (rev (trim_list_front (rev xs) a)) a"

value "trim_list ''  abc  def '' (CHR '' '')"


fun trim_string :: "string \<Rightarrow> string" where
  "trim_string s = trim_list s (CHR '' '')"


fun split_lines :: "string \<Rightarrow> string list" where
  "split_lines input = split_list input (char_of_nat 10)"


value "split_lines ''Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11''"


value "(10::nat) ^ 2::nat"



fun nat_of_string' :: "string \<Rightarrow> nat" where
  "nat_of_string' [] = 0"
| "nat_of_string' (x#[]) = nat_of_char' x"
| "nat_of_string' (x#xs) = (nat_of_char' x) * (10 ^ (length xs)) + (nat_of_string' xs)"

fun nat_of_string :: "string \<Rightarrow> nat" where
  "nat_of_string s = foldl (\<lambda> acc digit. acc * 10 + nat_of_char' digit) 0 s"


value "123::nat"
value "nat_of_string ''1232''"
value "foldl (\<lambda> acc digit. acc * 10 + nat_of_char' digit) 0 ''123''"
value "nat_of_string' ''123''"



(* lemma "nat_of_string s = n \<Longrightarrow> nat_of_string (char_of_nat d)#s = " *)


lemma "nat_of_string s = nat_of_string' s"
proof (induction s)
  case Nil
  then show ?case by simp
next
  case (Cons a s)
  then show ?case
    apply auto
    sorry
qed



end