palette(Colors):- Colors = [red, green, blue, cyan].

color_all(Map, Coloring) :- 
        append(Map, Vs),
        sort(Vs, OStates),
        random_permutation(OStates, States),
        bagof([S,_], member(S, States), Coloring),
        coloring_ok(States, Map, Coloring).

coloring_ok([], _, _).
coloring_ok([S|States], Map, Coloring) :-
        palette(Palette),
        member(MyColor, Palette),
        get_colors_from_states(Coloring, [S], [MyColor]),
        adjacent_ok(Map, S, MyColor, Coloring),
        coloring_ok(States, Map, Coloring).

adjacent_ok(Map, State, MyColor, Coloring) :-
        get_adjacent_states(State, Map, AdjStates),
        get_colors_from_states(Coloring, AdjStates, Colors),
        colors_unique(MyColor, Colors).

get_adjacent_states(State, Map, AdjStates) :-
        setof(S, (member([State,S], Map); member([S,State], Map)), AdjStates).

get_colors_from_states(Coloring, States, Colors) :-
        bagof(C, S^(member(S, States), member([S,C], Coloring)), Colors).

colors_unique(_, []).
colors_unique(MyColor, [C|Colors]) :-
        (var(C) ; MyColor \= C),
        colors_unique(MyColor, Colors).

romandie(Map) :- Map = [
         [jura, neuchatel],
         [neuchatel, fribourg], [neuchatel, vaud],
         [fribourg, vaud],
         [vaud, geneve], [vaud, valais] ].
usa_map(Map) :- Map= [
      [ca,az],[ca,nv],[ca,or],[or,wa],[wa,id],[or,id],
			[nv,id],[nv,az],[az,ut],[nv,ut],[ut,id],[ut,wy],
			[ut,co],[id,mt],[mt,wy],[id,wy],[wy,co],[az,co],
			[ut,nm],[nm,az],[nm,co],[mt,nd],[mt,sd],[nd,sd], 
			[wy,sd],[wy,nb],[nb,co],[nb,sd],[nb,ks],[co,ks],
			[co,ok],[ks,ok],[tx,ok],[nm,ok],[nm,tx],[nd,mn], 
			[sd,mn],[sd,ia],[mn,ia],[nb,ia],[nb,mo],[mo,ia],
			[mo,ks],[mo,ok],[mo,ar],[ok,ar],[ar,tx],[ar,la],
			[tx,la],[mn,wi],[ia,wi],[wi,il],[ia,il],[mo,il],
			[il,ky],[mo,ky],[ar,tn],[tn,ky],[ar,ms],[ms,la],
			[ms,tn],[mi,wi],[in,il],[in,mi],[in,ky],[tn,al],
			[al,ms],[mi,oh],[oh,in],[oh,ky],[ky,va],[ky,wv],
			[oh,wv],[tn,va],[tn,ga],[tn,nc],[ga,al],[ga,nc],
			[oh,pa],[al,fl],[ga,fl],[ga,sc],[nc,sc],[va,nc],
			[wv,va],[md,de],[md,wv],[md,va],[md,pa],[wv,pa],
			[de,pa],[de,nj],[pa,ny],[pa,nj],[ny,nj],[ny,ct],
			[ny,ma],[ma,ct],[ma,ri],[ct,ri],[ny,vt],[vt,nh],
			[ma,vt],[ma,nh],[me,nh],[nv,or] ]. 

