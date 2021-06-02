%Diogo Godinho Melita - ist199202 - LEIC-A Projeto LP 2020/2021
:-[codigo_comum].

%===============================================================================

% ---------------------------combinacoes_soma/4 ---------------------------
%combinacoes_soma(N, Els, Soma, Combs), em que N eh um inteiro, Els eh uma
%lista de inteiros, e Soma e um inteiro, significa que Combs eh a lista ordenada 
%cujos elementos sao as combinacoes N a N, dos elementos de Els cuja soma e Soma

combinacoes_soma(N, Els, Soma, Combs):-
  findall(Comb, (combinacao(N, Els, Comb), sum_list(Comb, Sum), Sum =:= Soma), 
  Combs).

%===============================================================================

% --------------------------permutacoes_soma/4 -----------------------------
%permutacoes_soma(N, Els, Soma, Perms), em que N eh um inteiro, Els eh uma
%lista de inteiros, e Soma eh um inteiro, significa que Perms eh a lista
%ordenada cujos elementos sao as permutacoes das combinacoes N a N, dos 
%elementos de Els cuja soma eh Soma .

permutacoes_soma(N, Els, Soma, Perms):-
  combinacoes_soma(N, Els, Soma, Permutacoes),
  findall(X, (member(P, Permutacoes), permutation(P, X)), Permutations),
  sort(Permutations, Perms).

%===============================================================================

%--------------------------espaco_fila/2 ---------------------------------------
%espaco_fila(Fila, Esp, H_V), em que Fila e uma fila (linha ou coluna) de um
%puzzle e H_V eh um dos atomos h ou v, conforme se trate de uma fila horizontal 
%ou vertical, respectivamente, significa que Esp e um espaco de Fila.

espaco_fila(Fila, Esps, H_V):-
  aux_ef(Fila, H_V, [], _, [], Final),
  member(Esps, Final).

%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

%------- aux_ef/6 ----------------- ef = espaco_fila
%aux_ef e um predicado auxiliar que funciona recursivamente e vai analisando 
%o primeiro mmebro da fila trantando de forma diferente caso seja uma variavel
% ou uma lista. A medida que se vai construindo um espaco, vai se adicionando as 
%variaveis todas a uma lista para que, quando se chegue ao fim do espaco, 
%tenhamos a lista de variaveis completas. A medida que e feito um espaco este e 
%adicionado a um acumulador sendo que quando chegamos ao fim, [], este 
%acumulador e passado para Final, sendo final a lista de espacos daquela fila.

%Le = Lista de Espacos

aux_ef([P|R], H_V, Ac, espaco(_, Vars), Le, Final):- 
  is_list(P),
  Ac == [],
  H_V == h,
  nth0(1, P, Sum), %caso H_V seja h o que vai interessar e o indice 1 da lista
  aux_ef(R, H_V, Ac, espaco(Sum, Vars), Le, Final).

aux_ef([P|R], H_V, Ac, espaco(_, Vars), Le, Final):-
  is_list(P),
  Ac == [],
  H_V == v, 
  nth0(0, P, Sum), %caso H_V seja v o que vai interessar e o indice 0 da lista
  aux_ef(R, H_V, Ac, espaco(Sum, Vars), Le, Final).

aux_ef([P|R], H_V, Ac, espaco(Soma, Vars), Le, Final):-
  is_list(P),
  Ac \== [], %significa que o acumulador tem variaveis logo e necessario
  faz_espaco(Soma, Ac, espaco(Soma, Vars)), %fazer um espaco
  append(Le, [espaco(Soma, Vars)], ListaEspacos),
  H_V == h,
  nth0(1, P, Sum), %vai interessar o indice pois pode ser um novo espaco
  aux_ef(R, H_V, [], espaco(Sum, _), ListaEspacos, Final).

aux_ef([P|R], H_V, Ac, espaco(Soma, Vars), Le, Final):-
  is_list(P),
  Ac \== [], %significa que o acumulador tem variaveis logo e necessario
  faz_espaco(Soma, Ac, espaco(Soma, Vars)), %fazer um espaco
  append(Le, [espaco(Soma, Vars)], ListaEspacos),
  H_V == v,
  nth0(0, P, Sum), %vai interessar o indice pois pode ser um novo espaco
  aux_ef(R, H_V, [], espaco(Sum, _), ListaEspacos, Final).

aux_ef([P|R], H_V, Ac, espaco(Soma, Vars), Le, Final):-
  var(P), %caso seja uma var vamos adiciona-la a lista de Vars
  append(Ac, [P], Acumulador),
  aux_ef(R, H_V, Acumulador, espaco(Soma, Vars), Le, Final).

aux_ef([], _, Ac, espaco(Soma, Vars), Le, Final):-
  Ac \== [], %o ultimo elemento foi uma var logo o acumulador de vars esta nao
  %vazio logo e necessario criar um espaco, junta-lo a lista de espacos e no fim
  %devolver a lista de espacos em Final
  faz_espaco(Soma, Ac, espaco(Soma, Vars)),
  append(Le, [espaco(Soma, Vars)], Lista),
  append(Lista, [], Final).

aux_ef([], _, Ac, _, Le, Le):-
  Ac == [].
  %caso o acumulador esteja vazio significa que o ultimo elemento era um bloco
  %vazio logo e preciso devolver a lista de espacos no ultimo argumento pois
  %chegamos a lista vazia, []

%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

%---- faz_espaco/2 -----
%faz_espaco e um construtor de espacos
faz_espaco(Soma, Vars, espaco(Soma, Vars)).

%===============================================================================

%---------------------------- espacos_fila/3 ----------------------------
%espacos_fila(H_V, Fila, Espacos), em que Fila e uma fila (linha ou coluna) de
%uma grelha e H_V e um dos atomos h ou v, significa que Espacos e a lista de 
%todos os espacos de Fila, da esquerda para a direita.

espacos_fila(H_V, Fila, Esps):-
  bagof(Espaco, espaco_fila(Fila, Espaco, H_V), Esps), !.

espacos_fila(_, _, []). %caso nao haja espacos numa fila devolve []

%===============================================================================

%------------------------------- espacos_puzzle/2 ------------------------------
%espacos_puzzle(Puzzle, Espacos), em que Puzzle e um puzzle, significa que
%Espacos e a lista de espacos de Puzzle.

espacos_puzzle(Puzzle, Espacos):-
  bagof(Esps, Fila^(member(Fila, Puzzle), espacos_fila(h, Fila, Esps)), H),
  mat_transposta(Puzzle, Tr), %recorremos a matriz transposta para fazer com que
  %as filas passem a colunas e vice-versa de forma a conseguir obter os espacos
  %na vertical
  bagof(Espaco, Fila^(member(Fila, Tr), espacos_fila(v, Fila, Espaco)), V),
  append(H, V, Lista), 
  %juntar a lista de espacos na horizontal com os espacos na vertical
  delete(Lista, [], Aux), %garantir que nao existem as ocorrencias []
  %provinientes de espacos_fila
  append(Aux, Espacos).

%===============================================================================

%-------------------------- espacos_com_posicoes_comuns/3 ----------------------
%espacos_com_posicoes_comuns(Espacos, Esp, Esps_com), em que Espacos
%e uma lista de espacos e Esp e um espaco, significa que Esps_com e a lista de 
%espacos com variaveis em comum com Esp, exceptuando Esp.

espacos_com_posicoes_comuns(Espacos, Esp, Esps_com):-
  aux_pc(Espacos, Esp, [], Aux), !,
  delete(Aux, Esp, Esps_com). %remover o Esp da lista anterior e devolver uma
  %vez que no predicado auxiliar esp e adicionado a lista Aux

%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

%----------aux_pc/4 ---------- pc = posicoes_comuns
%aux_pc e o predicado auxiliar recusrivo que vai verificar se existe algum 
%membro da lista de vars que tambem pertenca a lista de vars de Esp, usando o 
%predicado nao_membro/2

aux_pc([espaco(Sum, Vars)|R], espaco(Soma, VarsEsp), Ac, Final):-
  member(X, Vars),
  \+nao_membro(X, VarsEsp), %\+nao_membro = membro -> verificar se X tambem 
  %pertence a VarsEsp e se sim adicionamos ao acumulador
  append(Ac, [espaco(Sum, Vars)], Acumulador),
  aux_pc(R, espaco(Soma, VarsEsp), Acumulador, Final).

aux_pc([], _, Ac, Ac). %quando chegamos ao fim devolvemos o acumulador no final

aux_pc([espaco(_, Vars)|R], espaco(_, VarsEsp), Ac, Final):-
  member(X, Vars),
  nao_membro(X, VarsEsp), %caso nao seja membro passamos a frente
  aux_pc(R, espaco(_, VarsEsp), Ac, Final).

%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

%------------ nao_membro/2 ---------
%nao_membro e um predicado auxiliar que funciona como o member mas nao unifica
nao_membro(X, [Y|R]):-
  Y \== X,
  nao_membro(X, R).

nao_membro(_, []):- !.

%===============================================================================

%--------------------- permutacoes_soma_espacos/2 -----------------------------
%permutacoes_soma_espacos(Espacos, Perms_soma), em que Espacos e uma
%lista de espacos, significa que Perms_soma e a lista de listas de 2 elementos, 
%em que o 1o elemento e um espaco de Espacos e o 2o e a lista ordenada de 
%permutacoes cuja soma e igual a soma do espaco.

permutacoes_soma_espacos(Espacos, Perms_soma):-
  aux_pse(Espacos, [], Perms_soma).

%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

%---------aux_pse/3 -------- pse = permutacoes_soma_espaco
%aux_pse e um predicado auxiliar recursivo que utiliza um acumulador para ir 
%juntando todos os membros do tipo [Espaco, Perms] ate a lista ser [] e, nessa
%situacao devolve se o acumulador em Final

aux_pse([espaco(Soma, Vars)|R], Ac, Final):-
  length(Vars, L),
  permutacoes_soma(L, [1, 2, 3, 4, 5, 6, 7, 8, 9], Soma, Aux),
  append(Ac, [[espaco(Soma, Vars), Aux]], Acumulador),
  aux_pse(R, Acumulador, Final).

aux_pse([], Ac, Ac). %situacao final

%===============================================================================

%------------------- permutacao_possivel_espaco/3 ------------------------------
%permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma), em que
%Perm e uma permutacao, Esp e um espaco, Espacos e uma lista de espacos, e
%Perms_soma e uma lista de listas tal como obtida pelo predicado anterior, 
%significa que Perm e uma permutacao possivel para o espaco Esp.

permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma):-
  espacos_com_posicoes_comuns(Espacos, Esp, Comuns),
  member(PS, Perms_soma), %PS = Perm_Soma
  PS = [Esp, Permutacoes],
  member(Perm, Permutacoes),
  Esp = espaco(_, Perm),
  aux_ppe_junta_perms_comuns(Comuns, [], Perms_soma, ListaPermsComuns),
  maplist(aux_verificar_unificacao, ListaPermsComuns).

%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

%----aux_ppe_junta_perms_comuns/4---- ppe = permutacao_possivel_espaco
%aux_ppe_junta_perms_comuns e um predicado auxiliar que vai juntar todas as 
%perms comuns ao espaco

aux_ppe_junta_perms_comuns([], Ac, _, Ac).

aux_ppe_junta_perms_comuns([P|R], Ac, Perms_soma, Final):-
  member(PS, Perms_soma),
  PS = [P, _],
  append(Ac, [PS], Acumulador),
  aux_ppe_junta_perms_comuns(R, Acumulador, Perms_soma, Final).

%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

%------ aux_verificar_unificacao/1 ---------
%predicado auxiliar que vai testar a unificacao
aux_verificar_unificacao(PermsComuns):-
  PermsComuns = [espaco(_, Perm), Permutacoes],
  member(Perm, Permutacoes), !.

%===============================================================================

%------------------------ permutacoes_possiveis_espaco/3 ---------------------
%permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp,
%Perms_poss), em que Espacos e uma lista de espacos, Perms_soma e uma lista
%de listas tal como obtida pelo predicado permutacoes_soma_espacos, e Esp e um
%espaco, significa que Perms_poss e uma lista de 2 elementos em que o primeiro e
%a lista de variaveis de Esp e o segundo e a lista ordenada de permutacoes 
%possiveis para o espaco Esp.

permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss):-
  findall(Perm, (permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma)), 
  Aux),
  Esp = espaco(_, Vars),
  list_to_set(Aux, Auxiliar),
  append([Vars], [Auxiliar], Perms_poss), !.

permutacoes_possiveis_espaco(_, _, _, []).

%===============================================================================

%------------------ permutacoes_possiveis_espacos/2 ----------------------------
%permutacoes_possiveis_espacos(Espacos, Perms_poss_esps), em que
%Espacos e uma lista de espacos, significa que Perms_poss_esps e a lista de 
%permutacoes possiveis.

permutacoes_possiveis_espacos(Espacos, Perms_poss_esps):-
  permutacoes_soma_espacos(Espacos, Perms_soma),
  bagof(Perms_poss, Esp^(member(Esp, Espacos), 
  permutacoes_possiveis_espaco(Espacos,
  Perms_soma, Esp, Perms_poss)), Perms_poss_esps).

%===============================================================================

%--------------------------------- numeros_comuns/2 ---------------------------
%numeros_comuns(Lst_Perms, Numeros_comuns), em que Lst_Perms e uma lista
%de permutacoes, significa que Numeros_comuns e uma lista de pares (pos, numero)
%significando que todas as listas de Lst_Perms contem o numero numero na posicao
%pos.

numeros_comuns([P|Lst_Perms], Numeros_comuns):-
  findall((I, X), (nth1(I, P, X), aux_nc(Lst_Perms, X, I)), Numeros_comuns).

%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

%----aux_nc/3 ----- nc = numeros_comuns
%aux_nc e um predicado auxiliar que verificar se o valor X em P no indice I e o
%mesmo que o N

aux_nc([P|R], N, I):-
  nth1(I, P, X),
  N == X,
  aux_nc(R, N, I).

aux_nc([], _, _).

%===============================================================================

%------------------------------ atribui_comuns/1 -------------------------------
%atribui_comuns(Perms_Possiveis), em que Perms_Possiveis e uma lista de
%permutacoes possiveis, actualiza esta lista atribuindo a cada espaco numeros
%comuns a todas as permutacoes possiveis para esse espaco.

atribui_comuns(Perms_poss_esps):-
  aux_ac(Perms_poss_esps).

%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%--- aux_ac/1 --- ac = atribui_comuns
%aux_ac e um predicado auxiliar recursivo que vai modificar a lista a medida que
%sao feitas alteracoes

aux_ac([P|R]):-
  P = [Vars, LstPerms],
  numeros_comuns(LstPerms, Numeros_comuns),
  atribui(Numeros_comuns, Vars),
  aux_ac(R).

aux_ac([]).

%---- atribui/2 ----
%atribui e um predicado auxiliar que vai atribuir a cada var especifica o valor
%respetivo no indice correto
atribui([P|R], Vars):-
  P = (Indice, Valor),
  nth1(Indice, Vars, Var),
  Var = Valor,
  atribui(R, Vars).

atribui([], _).

%===============================================================================

%--------------------- retira_impossiveis/2 -----------------------------------
%retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis), em
%que Perms_Possiveis e uma lista de permutacoes possiveis, significa que
%Novas_Perms_Possiveis e o resultado de tirar permutacoes impossiveis de
%Perms_Possiveis.

retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis):-
  aux_retiraimpossiveis(Perms_Possiveis, [], Novas_Perms_Possiveis).

%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

%--------aux_retiraimpossiveis/3 ----
%aux_retiraimpossiveis e um predicado auxiliar recursivo que vai, usando o 
%exclude, excluir as perms que nao unifiquem com as vars, uma vez que alguns
%valores destas ja estao atribuidos, e guardando o resultado numa Lista de Perms
%atualizada e a medida que acaba a execucao deste processo, vai adicionando as 
%modificacoes a um acumulador, passando para Final quando chegamos a [].

aux_retiraimpossiveis([P|R], Ac, Final):-
  P = [Vars, LstPerms],
  exclude(\=(Vars), LstPerms, LstPermsAtualizada),
  append(Ac, [[Vars, LstPermsAtualizada]], Acumulador),
  aux_retiraimpossiveis(R, Acumulador, Final).

aux_retiraimpossiveis([], Ac, Ac).

%===============================================================================

%--------------------- simplifica/2 -------------------------------------------
%simplifica(Perms_Possiveis, Novas_Perms_Possiveis), em que
%Perms_Possiveis e uma lista de permutacoes possiveis, significa que
%Novas_Perms_Possiveis e o resultado de simplificar Perms_Possiveis.

simplifica(Perms_Possiveis, Novas_Perms_Possiveis):-
  atribui_comuns(Perms_Possiveis),
  retira_impossiveis(Perms_Possiveis, Aux),
  Perms_Possiveis \== Aux, !, %a condicao de paragem e PermsPossiveis == Aux
  simplifica(Aux, Novas_Perms_Possiveis).

simplifica(Perms_Possiveis, Novas_Perms_Possiveis):-
  atribui_comuns(Perms_Possiveis),
  retira_impossiveis(Perms_Possiveis, Aux),
  Novas_Perms_Possiveis = Aux.

%===============================================================================

%-------------------------------- inicializa/2 -----------------------------
%inicializa(Puzzle, Perms_Possiveis), em que Puzzle e um puzzle, significa que 
%Perms_Possiveis e a lista de permutacoes possiveis simplificada para Puzzle

inicializa(Puzzle, Perms_Possiveis):-
  espacos_puzzle(Puzzle, Espacos),
  permutacoes_possiveis_espacos(Espacos, PermsPoss),
  simplifica(PermsPoss, Perms_Possiveis).

%===============================================================================

% --------------------- escolhe_menos_aternativas/2 ---------------------------
%escolhe_menos_alternativas(Perms_Possiveis, Escolha), em que
%Perms_Possiveis e uma lista de permutacoes possiveis, significa que Escolha
%e o elemento de Perms_Possiveis escolhido segundo o criterio indicado na Seccao 
%2.2, no passo 1. Se todos os espacos em Perms_Possiveis tiverem associadas
%listas de permutacoes unitarias, o predicado deve devolver "falso".

escolhe_menos_alternativas(Perms_Possiveis, Escolha):-
  aux_ema(Perms_Possiveis, [], [], ListaLengths, LPM),
  min_list(ListaLengths, X),
  nth1(I, ListaLengths, X), !, %garantimos que obtemos a 1a ocorrencia de X
  nth1(I, LPM, Escolha).

%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

%---aux_ema/5 ---- ema = escolhe_menos_alternativas
%aux_ema e um predicado recursivo auxiliar que vai acumular num acumulador as
%lengths das Lst_Perms de P caso seja diferente de 1 e noutro acumulador os 
%P caso a length da sua Lst_Perms for diferente de 1 para depois saber qual sera
%a escolha mais facilmente

aux_ema([P|R], ListaLengths, LPM, Final1, Final2):-
  P = [_, Lst_Perms],
  length(Lst_Perms, N),
  N \== 1,
  append(ListaLengths, [N], Acumulador),
  append(LPM, [P], Ac),
  aux_ema(R, Acumulador, Ac, Final1, Final2).

aux_ema([P|R], ListaLengths, LPM, Final1, Final2):-
  P = [_, Lst_Perms],
  length(Lst_Perms, N),
  N == 1,
  aux_ema(R, ListaLengths, LPM, Final1, Final2).

aux_ema([], ListaLengths, LPM, ListaLengths, LPM).

%===============================================================================

%---------------------------- experimenta_perm/3 ------------------------------
%A chamada experimenta_perm(Escolha, Perms_Possiveis,
%Novas_Perms_Possiveis), em que Perms_Possiveis e uma lista de permutacoes
%possiveis, e Escolha e um dos seus elementos segue os seguintes passos:

%1. Sendo Esp e Lst_Perms o espaco e a lista de permutacoes de Escolha, 
%respectivamente, escolhe uma permutacao de Lst_Perms, Perm.

%2. Unificar Esp com Perm.

%3. Novas_Perms_Possiveis e o resultado de substituir, em Perms_Possiveis, o
%elemento Escolha pelo elemento [Esp, [Perm]].

experimenta_perm(Escolha, Perms_Possiveis, Novas_Perms_Possiveis):-
  Escolha = [Esp, Lst_Perms],
  member(Perm, Lst_Perms),
  Esp = Perm,
  substitui(Escolha, [Esp, [Perm]], Perms_Possiveis, Novas_Perms_Possiveis).

%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

% ----- substitui/4 ----
%substitui e um predicado auxiliar que vai substituir o elemento Antigo, pelo
%Novo em Perms_Possiveis devolvendo em Novas_Perms_Possiveis a substituicao

substitui(_, _, [], []).

substitui(Antigo, Novo, [Antigo|Res], [Novo|Res1]):-
  substitui(Antigo, Novo, Res, Res1).

substitui(Antigo, Novo, [X|Res], [X|Res1]):-
  X \== Antigo,
  substitui(Antigo, Novo, Res, Res1).

%===============================================================================

%---------------------------------- resolve_aux/2 -----------------------------
%resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis), em que
%Perms_Possiveis e uma lista de permutacoes possiveis, significa que
%Novas_Perms_Possiveis e o resultado de aplicar o algoritmo descrito na Seccao
%2.2 a Perms_Possiveis.

resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis):-
  escolhe_menos_alternativas(Perms_Possiveis, Escolha), !,
  experimenta_perm(Escolha, Perms_Possiveis, Aux),
  simplifica(Aux, Aux2),
  resolve_aux(Aux2, Novas_Perms_Possiveis).

resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis):-
  simplifica(Perms_Possiveis, Novas_Perms_Possiveis).

%===============================================================================

%--------------------------- resolve/1 -----------------------------------------
%resolve(Puz), em que Puz e um puzzle, resolve esse puzzle, isto e, apos a
%invocacao deste predicado a grelha de Puz tem todas as variaveis substituidas
%por numeros que respeitam as restricoes Puz.

resolve(Puz):-
  inicializa(Puz, Perms_Possiveis),
  resolve_aux(Perms_Possiveis, _).

%===============================================================================