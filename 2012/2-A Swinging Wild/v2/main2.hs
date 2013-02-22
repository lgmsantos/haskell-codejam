[1 of 1] Compiling Main             ( main.hs, main.o )

==================== Tidy Core ====================
Result size = 1836

Main.next1 :: Main.Vine
[GblId, Str=DmdType b]
Main.next1 = Control.Exception.Base.recSelError @ Main.Vine "next"

Main.next :: Main.Vine -> Main.Vine
[GblId[[RecSel]],
 Arity=1,
 Str=DmdType S,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Arity=1, Value=True,
         ConLike=True, Cheap=True, Expandable=True,
         Guidance=ALWAYS_IF(unsat_ok=True,boring_ok=True)}]
Main.next =
  \ (ds_dXu :: Main.Vine) ->
    case ds_dXu of _ {
      Main.Vine ds1_dXv ds2_dXw ds3_dXx -> ds3_dXx;
      Main.None -> Main.next1
    }

Rec {
asList_r3cB :: Main.Vine -> [Main.Vine]
[GblId, Arity=1, Caf=NoCafRefs, Str=DmdType S]
asList_r3cB =
  \ (ds_dWD :: Main.Vine) ->
    case ds_dWD of wild_XR {
      Main.Vine ipv_sYi ipv1_sYj ipv2_sYk ->
        GHC.Types.: @ Main.Vine wild_XR (asList_r3cB ipv2_sYk);
      Main.None -> GHC.Types.[] @ Main.Vine
    }
end Rec }

Main.height1 :: GHC.Types.Int
[GblId, Str=DmdType b]
Main.height1 =
  Control.Exception.Base.recSelError @ GHC.Types.Int "height"

Main.height :: Main.Vine -> GHC.Types.Int
[GblId[[RecSel]],
 Arity=1,
 Str=DmdType S,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Arity=1, Value=True,
         ConLike=True, Cheap=True, Expandable=True,
         Guidance=ALWAYS_IF(unsat_ok=True,boring_ok=True)}]
Main.height =
  \ (ds_dXn :: Main.Vine) ->
    case ds_dXn of _ {
      Main.Vine ds1_dXo ds2_dXp ds3_dXq -> ds2_dXp;
      Main.None -> Main.height1
    }

Main.position1 :: GHC.Types.Int
[GblId, Str=DmdType b]
Main.position1 =
  Control.Exception.Base.recSelError @ GHC.Types.Int "position"

Main.position :: Main.Vine -> GHC.Types.Int
[GblId[[RecSel]],
 Arity=1,
 Str=DmdType S,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Arity=1, Value=True,
         ConLike=True, Cheap=True, Expandable=True,
         Guidance=ALWAYS_IF(unsat_ok=True,boring_ok=True)}]
Main.position =
  \ (ds_dXg :: Main.Vine) ->
    case ds_dXg of _ {
      Main.Vine ds1_dXh ds2_dXi ds3_dXj -> ds1_dXh;
      Main.None -> Main.position1
    }

Main.$fShowVine2 :: [GHC.Types.Char]
[GblId,
 Str=DmdType,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Arity=0, Value=False,
         ConLike=False, Cheap=False, Expandable=False,
         Guidance=IF_ARGS [] 40 0}]
Main.$fShowVine2 = GHC.CString.unpackCString# "None"

Main.$fShowVine1 :: GHC.Show.ShowS
[GblId,
 Arity=1,
 Str=DmdType,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Arity=0, Value=True,
         ConLike=True, Cheap=True, Expandable=True,
         Guidance=IF_ARGS [] 20 60}]
Main.$fShowVine1 = GHC.Base.++ @ GHC.Types.Char Main.$fShowVine2

Main.$fShowVine3 :: GHC.Types.Char
[GblId,
 Caf=NoCafRefs,
 Str=DmdType m,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Arity=0, Value=True,
         ConLike=True, Cheap=True, Expandable=True,
         Guidance=IF_ARGS [] 10 20}]
Main.$fShowVine3 = GHC.Types.C# '}'

lvl_r3cC :: GHC.Types.Int
[GblId, Caf=NoCafRefs, Str=DmdType m]
lvl_r3cC = GHC.Types.I# 0

Main.$fShowVine4 :: [GHC.Types.Char]
[GblId,
 Str=DmdType,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Arity=0, Value=False,
         ConLike=False, Cheap=False, Expandable=False,
         Guidance=IF_ARGS [] 50 0}]
Main.$fShowVine4 = GHC.CString.unpackCString# "next = "

Main.$fShowVine6 :: [GHC.Types.Char]
[GblId,
 Str=DmdType,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Arity=0, Value=False,
         ConLike=False, Cheap=False, Expandable=False,
         Guidance=IF_ARGS [] 40 0}]
Main.$fShowVine6 = GHC.CString.unpackCString# ", "

Main.$fShowVine5 :: [GHC.Types.Char]
[GblId,
 Str=DmdType,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Arity=0, Value=False,
         ConLike=False, Cheap=False, Expandable=False,
         Guidance=IF_ARGS [] 60 0}]
Main.$fShowVine5 = GHC.CString.unpackCString# "height = "

Main.$fShowVine7 :: [GHC.Types.Char]
[GblId,
 Str=DmdType,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Arity=0, Value=False,
         ConLike=False, Cheap=False, Expandable=False,
         Guidance=IF_ARGS [] 60 0}]
Main.$fShowVine7 = GHC.CString.unpackCString# "position = "

Main.$fShowVine8 :: [GHC.Types.Char]
[GblId,
 Str=DmdType,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Arity=0, Value=False,
         ConLike=False, Cheap=False, Expandable=False,
         Guidance=IF_ARGS [] 50 0}]
Main.$fShowVine8 = GHC.CString.unpackCString# "Vine {"

Rec {
Main.$fShowProblem_$s$cshowsPrec [Occ=LoopBreaker]
  :: GHC.Prim.Int# -> Main.Vine -> GHC.Show.ShowS
[GblId, Arity=2, Str=DmdType LS]
Main.$fShowProblem_$s$cshowsPrec =
  \ (sc_s38W :: GHC.Prim.Int#) (sc1_s38X :: Main.Vine) ->
    case sc1_s38X of _ {
      Main.Vine b1_at5 b2_at6 b3_at7 ->
        let {
          f_X150 [Dmd=Just L] :: GHC.Base.String -> GHC.Base.String
          [LclId, Str=DmdType]
          f_X150 = Main.$fShowProblem_$s$cshowsPrec 0 b3_at7 } in
        let {
          p_aYU :: GHC.Show.ShowS
          [LclId, Arity=1, Str=DmdType L]
          p_aYU =
            \ (x_X12q :: GHC.Base.String) ->
              GHC.Base.++
                @ GHC.Types.Char
                Main.$fShowVine8
                (GHC.Base.++
                   @ GHC.Types.Char
                   Main.$fShowVine7
                   (case b1_at5 of _ { GHC.Types.I# ww_a30h ->
                    GHC.Show.itos
                      ww_a30h
                      (GHC.Base.++
                         @ GHC.Types.Char
                         Main.$fShowVine6
                         (GHC.Base.++
                            @ GHC.Types.Char
                            Main.$fShowVine5
                            (case b2_at6 of _ { GHC.Types.I# ww1_X324 ->
                             GHC.Show.itos
                               ww1_X324
                               (GHC.Base.++
                                  @ GHC.Types.Char
                                  Main.$fShowVine6
                                  (GHC.Base.++
                                     @ GHC.Types.Char
                                     Main.$fShowVine4
                                     (f_X150
                                        (GHC.Types.: @ GHC.Types.Char Main.$fShowVine3 x_X12q))))
                             })))
                    })) } in
        case GHC.Prim.>=# sc_s38W 11 of _ {
          GHC.Types.False -> p_aYU;
          GHC.Types.True ->
            \ (x_aYZ :: GHC.Base.String) ->
              GHC.Types.:
                @ GHC.Types.Char
                GHC.Show.shows2
                (p_aYU (GHC.Types.: @ GHC.Types.Char GHC.Show.shows1 x_aYZ))
        };
      Main.None -> Main.$fShowVine1
    }
end Rec }

Main.$fShowVine_$cshowsPrec
  :: GHC.Types.Int -> Main.Vine -> GHC.Show.ShowS
[GblId,
 Arity=2,
 Str=DmdType LS,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Arity=2, Value=True,
         ConLike=True, Cheap=True, Expandable=True,
         Guidance=IF_ARGS [20 30] 451 180}]
Main.$fShowVine_$cshowsPrec =
  \ (a_at4 :: GHC.Types.Int) (ds_dXB :: Main.Vine) ->
    case ds_dXB of _ {
      Main.Vine b1_at5 b2_at6 b3_at7 ->
        case a_at4 of _ { GHC.Types.I# x_aZ5 ->
        let {
          f_X150 [Dmd=Just L] :: GHC.Base.String -> GHC.Base.String
          [LclId, Str=DmdType]
          f_X150 = Main.$fShowProblem_$s$cshowsPrec 0 b3_at7 } in
        let {
          p_aYU :: GHC.Show.ShowS
          [LclId, Arity=1, Str=DmdType L]
          p_aYU =
            \ (x1_X12q :: GHC.Base.String) ->
              GHC.Base.++
                @ GHC.Types.Char
                Main.$fShowVine8
                (GHC.Base.++
                   @ GHC.Types.Char
                   Main.$fShowVine7
                   (case b1_at5 of _ { GHC.Types.I# ww_a30h ->
                    GHC.Show.itos
                      ww_a30h
                      (GHC.Base.++
                         @ GHC.Types.Char
                         Main.$fShowVine6
                         (GHC.Base.++
                            @ GHC.Types.Char
                            Main.$fShowVine5
                            (case b2_at6 of _ { GHC.Types.I# ww1_X324 ->
                             GHC.Show.itos
                               ww1_X324
                               (GHC.Base.++
                                  @ GHC.Types.Char
                                  Main.$fShowVine6
                                  (GHC.Base.++
                                     @ GHC.Types.Char
                                     Main.$fShowVine4
                                     (f_X150
                                        (GHC.Types.: @ GHC.Types.Char Main.$fShowVine3 x1_X12q))))
                             })))
                    })) } in
        case GHC.Prim.>=# x_aZ5 11 of _ {
          GHC.Types.False -> p_aYU;
          GHC.Types.True ->
            \ (x1_aYZ :: GHC.Base.String) ->
              GHC.Types.:
                @ GHC.Types.Char
                GHC.Show.shows2
                (p_aYU (GHC.Types.: @ GHC.Types.Char GHC.Show.shows1 x1_aYZ))
        }
        };
      Main.None -> Main.$fShowVine1
    }

Main.$fShowVine_$cshowList :: [Main.Vine] -> GHC.Show.ShowS
[GblId,
 Arity=2,
 Str=DmdType SL,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Arity=2, Value=True,
         ConLike=True, Cheap=True, Expandable=True,
         Guidance=IF_ARGS [80 0] 270 30}]
Main.$fShowVine_$cshowList =
  \ (ds1_aYs :: [Main.Vine]) (s_aYt :: GHC.Base.String) ->
    case ds1_aYs of _ {
      [] -> GHC.CString.unpackAppendCString# "[]" s_aYt;
      : x_aYy xs_aYz ->
        GHC.Types.:
          @ GHC.Types.Char
          GHC.Show.showList__3
          (Main.$fShowProblem_$s$cshowsPrec
             0
             x_aYy
             (let {
                lvl19_aYB :: [GHC.Types.Char]
                [LclId, Str=DmdType]
                lvl19_aYB =
                  GHC.Types.: @ GHC.Types.Char GHC.Show.showList__2 s_aYt } in
              letrec {
                showl_aYC [Occ=LoopBreaker] :: [Main.Vine] -> [GHC.Types.Char]
                [LclId, Arity=1, Str=DmdType S]
                showl_aYC =
                  \ (ds2_aYD :: [Main.Vine]) ->
                    case ds2_aYD of _ {
                      [] -> lvl19_aYB;
                      : y_aYI ys_aYJ ->
                        GHC.Types.:
                          @ GHC.Types.Char
                          GHC.Show.showList__1
                          (Main.$fShowProblem_$s$cshowsPrec 0 y_aYI (showl_aYC ys_aYJ))
                    }; } in
              showl_aYC xs_aYz))
    }

Main.$fShowVine_$cshow :: Main.Vine -> GHC.Base.String
[GblId,
 Arity=1,
 Str=DmdType S,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Arity=1, Value=True,
         ConLike=True, Cheap=True, Expandable=True,
         Guidance=IF_ARGS [0] 40 0}]
Main.$fShowVine_$cshow =
  \ (x_aYP :: Main.Vine) ->
    Main.$fShowProblem_$s$cshowsPrec
      0 x_aYP (GHC.Types.[] @ GHC.Types.Char)

Main.$fShowVine [InlPrag=[ALWAYS] CONLIKE]
  :: GHC.Show.Show Main.Vine
[GblId[DFunId],
 Str=DmdType m,
 Unf=DFun(arity=0) GHC.Show.D:Show [Main.$fShowVine_$cshowsPrec,
                                    Main.$fShowVine_$cshow, Main.$fShowVine_$cshowList]]
Main.$fShowVine =
  GHC.Show.D:Show
    @ Main.Vine
    Main.$fShowVine_$cshowsPrec
    Main.$fShowVine_$cshow
    Main.$fShowVine_$cshowList

Main.$fShowProblem1 :: [GHC.Types.Char]
[GblId,
 Str=DmdType,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Arity=0, Value=False,
         ConLike=False, Cheap=False, Expandable=False,
         Guidance=IF_ARGS [] 50 0}]
Main.$fShowProblem1 = GHC.CString.unpackCString# "Problem "

Main.$w$cshowsPrec
  :: GHC.Prim.Int# -> Main.Vine -> GHC.Types.Int -> GHC.Show.ShowS
[GblId,
 Arity=3,
 Str=DmdType LLL,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Arity=3, Value=True,
         ConLike=True, Cheap=True, Expandable=True,
         Guidance=IF_ARGS [0 0 20] 241 120}]
Main.$w$cshowsPrec =
  \ (ww_s366 :: GHC.Prim.Int#)
    (ww1_s36a :: Main.Vine)
    (ww2_s36b :: GHC.Types.Int) ->
    let {
      f_X123 [Dmd=Just L] :: GHC.Base.String -> GHC.Base.String
      [LclId, Str=DmdType]
      f_X123 = Main.$fShowProblem_$s$cshowsPrec 11 ww1_s36a } in
    let {
      p_aYU :: GHC.Show.ShowS
      [LclId, Arity=1, Str=DmdType L]
      p_aYU =
        \ (x_X12a :: GHC.Base.String) ->
          GHC.Base.++
            @ GHC.Types.Char
            Main.$fShowProblem1
            (f_X123
               (GHC.Types.:
                  @ GHC.Types.Char
                  GHC.Show.shows14
                  (case ww2_s36b of _ { GHC.Types.I# ww3_a30h ->
                   GHC.Show.$wshowSignedInt 11 ww3_a30h x_X12a
                   }))) } in
    case GHC.Prim.>=# ww_s366 11 of _ {
      GHC.Types.False -> p_aYU;
      GHC.Types.True ->
        \ (x_aYZ :: GHC.Base.String) ->
          GHC.Types.:
            @ GHC.Types.Char
            GHC.Show.shows2
            (p_aYU (GHC.Types.: @ GHC.Types.Char GHC.Show.shows1 x_aYZ))
    }

Main.$fShowProblem_$cshowsPrec [InlPrag=INLINE[0]]
  :: GHC.Types.Int -> Main.Problem -> GHC.Show.ShowS
[GblId,
 Arity=2,
 Str=DmdType U(L)U(LL),
 Unf=Unf{Src=Worker=Main.$w$cshowsPrec, TopLvl=True, Arity=2,
         Value=True, ConLike=True, Cheap=True, Expandable=True,
         Guidance=ALWAYS_IF(unsat_ok=True,boring_ok=False)
         Tmpl= \ (w_s364 [Occ=Once!] :: GHC.Types.Int)
                 (w1_s368 [Occ=Once!] :: Main.Problem) ->
                 case w_s364 of _ { GHC.Types.I# ww_s366 [Occ=Once] ->
                 case w1_s368
                 of _ { Main.Problem ww1_s36a [Occ=Once] ww2_s36b [Occ=Once] ->
                 Main.$w$cshowsPrec ww_s366 ww1_s36a ww2_s36b
                 }
                 }}]
Main.$fShowProblem_$cshowsPrec =
  \ (w_s364 :: GHC.Types.Int) (w1_s368 :: Main.Problem) ->
    case w_s364 of _ { GHC.Types.I# ww_s366 ->
    case w1_s368 of _ { Main.Problem ww1_s36a ww2_s36b ->
    Main.$w$cshowsPrec ww_s366 ww1_s36a ww2_s36b
    }
    }

Main.$fShowProblem_$cshowList :: [Main.Problem] -> GHC.Show.ShowS
[GblId,
 Arity=2,
 Str=DmdType SL,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Arity=2, Value=True,
         ConLike=True, Cheap=True, Expandable=True,
         Guidance=IF_ARGS [80 0] 310 30}]
Main.$fShowProblem_$cshowList =
  \ (ds1_aYs :: [Main.Problem]) (s_aYt :: GHC.Base.String) ->
    case ds1_aYs of _ {
      [] -> GHC.CString.unpackAppendCString# "[]" s_aYt;
      : x_aYy xs_aYz ->
        GHC.Types.:
          @ GHC.Types.Char
          GHC.Show.showList__3
          (case x_aYy of _ { Main.Problem ww_s36a ww1_s36b ->
           Main.$w$cshowsPrec
             0
             ww_s36a
             ww1_s36b
             (let {
                lvl19_aYB :: [GHC.Types.Char]
                [LclId, Str=DmdType]
                lvl19_aYB =
                  GHC.Types.: @ GHC.Types.Char GHC.Show.showList__2 s_aYt } in
              letrec {
                showl_aYC [Occ=LoopBreaker] :: [Main.Problem] -> [GHC.Types.Char]
                [LclId, Arity=1, Str=DmdType S]
                showl_aYC =
                  \ (ds2_aYD :: [Main.Problem]) ->
                    case ds2_aYD of _ {
                      [] -> lvl19_aYB;
                      : y_aYI ys_aYJ ->
                        GHC.Types.:
                          @ GHC.Types.Char
                          GHC.Show.showList__1
                          (case y_aYI of _ { Main.Problem ww2_X38f ww3_X3an ->
                           Main.$w$cshowsPrec 0 ww2_X38f ww3_X3an (showl_aYC ys_aYJ)
                           })
                    }; } in
              showl_aYC xs_aYz)
           })
    }

Main.$fShowProblem_$cshow :: Main.Problem -> GHC.Base.String
[GblId,
 Arity=1,
 Str=DmdType U(LL),
 Unf=Unf{Src=InlineStable, TopLvl=True, Arity=1, Value=True,
         ConLike=True, Cheap=True, Expandable=True,
         Guidance=ALWAYS_IF(unsat_ok=True,boring_ok=False)
         Tmpl= \ (x_aYP [Occ=Once] :: Main.Problem) ->
                 Main.$fShowProblem_$cshowsPrec
                   GHC.Base.zeroInt x_aYP (GHC.Types.[] @ GHC.Types.Char)}]
Main.$fShowProblem_$cshow =
  \ (x_aYP :: Main.Problem) ->
    case x_aYP of _ { Main.Problem ww_s36a ww1_s36b ->
    Main.$w$cshowsPrec
      0 ww_s36a ww1_s36b (GHC.Types.[] @ GHC.Types.Char)
    }

Main.$fShowProblem [InlPrag=[ALWAYS] CONLIKE]
  :: GHC.Show.Show Main.Problem
[GblId[DFunId],
 Str=DmdType m,
 Unf=DFun(arity=0) GHC.Show.D:Show [Main.$fShowProblem_$cshowsPrec,
                                    Main.$fShowProblem_$cshow, Main.$fShowProblem_$cshowList]]
Main.$fShowProblem =
  GHC.Show.D:Show
    @ Main.Problem
    Main.$fShowProblem_$cshowsPrec
    Main.$fShowProblem_$cshow
    Main.$fShowProblem_$cshowList

Main.$fOrdProblem_$ccompare1
  :: Main.Vine -> Main.Vine -> GHC.Types.Ordering
[GblId,
 Arity=2,
 Caf=NoCafRefs,
 Str=DmdType SS,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Arity=2, Value=True,
         ConLike=True, Cheap=True, Expandable=True,
         Guidance=IF_ARGS [50 60] 110 30}]
Main.$fOrdProblem_$ccompare1 =
  \ (ds_dXY :: Main.Vine) (ds1_dXZ :: Main.Vine) ->
    case ds_dXY of _ {
      Main.Vine ipv_sZw ipv1_sZx ipv2_sZy ->
        case ds1_dXZ of _ {
          Main.Vine ipv3_sZB ipv4_sZC ipv5_sZD ->
            case ipv_sZw of _ { GHC.Types.I# x#_aZJ ->
            case ipv3_sZB of _ { GHC.Types.I# y#_aZN ->
            GHC.Classes.compareInt# x#_aZJ y#_aZN
            }
            };
          Main.None -> GHC.Types.GT
        };
      Main.None ->
        case ds1_dXZ of _ {
          Main.Vine ipv_sZQ ipv1_sZR ipv2_sZS -> GHC.Types.LT;
          Main.None -> GHC.Types.EQ
        }
    }

Rec {
Main.$fEqProblem_$c==1 [Occ=LoopBreaker]
  :: Main.Vine -> Main.Vine -> GHC.Types.Bool
[GblId, Arity=2, Caf=NoCafRefs, Str=DmdType SS]
Main.$fEqProblem_$c==1 =
  \ (ds_dXE :: Main.Vine) (ds1_dXF :: Main.Vine) ->
    case ds_dXE of _ {
      Main.Vine a1_at8 a2_at9 a3_ata ->
        case ds1_dXF of _ {
          Main.Vine b1_atb b2_atc b3_atd ->
            case a1_at8 of _ { GHC.Types.I# x_a10b ->
            case b1_atb of _ { GHC.Types.I# y_a10f ->
            case GHC.Prim.==# x_a10b y_a10f of _ {
              GHC.Types.False -> GHC.Types.False;
              GHC.Types.True ->
                case a2_at9 of _ { GHC.Types.I# x1_X13o ->
                case b2_atc of _ { GHC.Types.I# y1_X13w ->
                case GHC.Prim.==# x1_X13o y1_X13w of _ {
                  GHC.Types.False -> GHC.Types.False;
                  GHC.Types.True -> Main.$fEqProblem_$c==1 a3_ata b3_atd
                }
                }
                }
            }
            }
            };
          Main.None -> GHC.Types.False
        };
      Main.None ->
        case ds1_dXF of _ {
          Main.Vine ds2_dXb ds3_dXc ds4_dXd -> GHC.Types.False;
          Main.None -> GHC.Types.True
        }
    }
end Rec }

Main.$fEqVine_$c/= :: Main.Vine -> Main.Vine -> GHC.Types.Bool
[GblId,
 Arity=2,
 Caf=NoCafRefs,
 Str=DmdType SS,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Arity=2, Value=True,
         ConLike=True, Cheap=True, Expandable=True,
         Guidance=IF_ARGS [0 0] 50 20}]
Main.$fEqVine_$c/= =
  \ (a_ati :: Main.Vine) (b_atj :: Main.Vine) ->
    case Main.$fEqProblem_$c==1 a_ati b_atj of _ {
      GHC.Types.False -> GHC.Types.True;
      GHC.Types.True -> GHC.Types.False
    }

Main.$fEqVine [InlPrag=[ALWAYS] CONLIKE]
  :: GHC.Classes.Eq Main.Vine
[GblId[DFunId],
 Caf=NoCafRefs,
 Str=DmdType m,
 Unf=DFun(arity=0) GHC.Classes.D:Eq [Main.$fEqProblem_$c==1,
                                     Main.$fEqVine_$c/=]]
Main.$fEqVine =
  GHC.Classes.D:Eq
    @ Main.Vine Main.$fEqProblem_$c==1 Main.$fEqVine_$c/=

Main.$fEqProblem_$c==
  :: Main.Problem -> Main.Problem -> GHC.Types.Bool
[GblId,
 Arity=2,
 Caf=NoCafRefs,
 Str=DmdType U(SA)U(SA),
 Unf=Unf{Src=InlineStable, TopLvl=True, Arity=2, Value=True,
         ConLike=True, Cheap=True, Expandable=True,
         Guidance=ALWAYS_IF(unsat_ok=True,boring_ok=False)
         Tmpl= \ (ds_dXS [Occ=Once!] :: Main.Problem)
                 (ds1_dXT [Occ=Once!] :: Main.Problem) ->
                 case ds_dXS of _ { Main.Problem v_aom [Occ=Once] _ ->
                 case ds1_dXT of _ { Main.Problem v'_aon [Occ=Once] _ ->
                 Main.$fEqProblem_$c==1 v_aom v'_aon
                 }
                 }}]
Main.$fEqProblem_$c== =
  \ (ds_dXS :: Main.Problem) (ds1_dXT :: Main.Problem) ->
    case ds_dXS of _ { Main.Problem v_aom ds2_dXU ->
    case ds1_dXT of _ { Main.Problem v'_aon ds3_dXV ->
    Main.$fEqProblem_$c==1 v_aom v'_aon
    }
    }

Main.$fEqProblem_$c/= [InlPrag=INLINE (sat-args=2)]
  :: Main.Problem -> Main.Problem -> GHC.Types.Bool
[GblId,
 Arity=2,
 Caf=NoCafRefs,
 Str=DmdType U(SA)U(SA),
 Unf=Unf{Src=InlineStable, TopLvl=True, Arity=2, Value=True,
         ConLike=True, Cheap=True, Expandable=True,
         Guidance=ALWAYS_IF(unsat_ok=False,boring_ok=False)
         Tmpl= \ (x_a10l [Occ=Once] :: Main.Problem)
                 (y_a10m [Occ=Once] :: Main.Problem) ->
                 GHC.Classes.not (Main.$fEqProblem_$c== x_a10l y_a10m)}]
Main.$fEqProblem_$c/= =
  \ (eta_B2 :: Main.Problem) (eta1_B1 :: Main.Problem) ->
    case eta_B2 of _ { Main.Problem v_aom ds_dXU ->
    case eta1_B1 of _ { Main.Problem v'_aon ds1_dXV ->
    case Main.$fEqProblem_$c==1 v_aom v'_aon of _ {
      GHC.Types.False -> GHC.Types.True;
      GHC.Types.True -> GHC.Types.False
    }
    }
    }

Main.$fEqProblem [InlPrag=[ALWAYS] CONLIKE]
  :: GHC.Classes.Eq Main.Problem
[GblId[DFunId],
 Caf=NoCafRefs,
 Str=DmdType m,
 Unf=DFun(arity=0) GHC.Classes.D:Eq [Main.$fEqProblem_$c==,
                                     Main.$fEqProblem_$c/=]]
Main.$fEqProblem =
  GHC.Classes.D:Eq
    @ Main.Problem Main.$fEqProblem_$c== Main.$fEqProblem_$c/=

Main.$fOrdProblem_$c<=1 :: Main.Vine -> Main.Vine -> GHC.Types.Bool
[GblId,
 Arity=2,
 Caf=NoCafRefs,
 Str=DmdType SS,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Arity=2, Value=True,
         ConLike=True, Cheap=True, Expandable=True,
         Guidance=IF_ARGS [50 60] 102 40}]
Main.$fOrdProblem_$c<=1 =
  \ (x_a10H :: Main.Vine) (y_a10I :: Main.Vine) ->
    case x_a10H of _ {
      Main.Vine ipv_sZw ipv1_sZx ipv2_sZy ->
        case y_a10I of _ {
          Main.Vine ipv3_sZB ipv4_sZC ipv5_sZD ->
            case ipv_sZw of _ { GHC.Types.I# x#_aZJ ->
            case ipv3_sZB of _ { GHC.Types.I# y#_aZN ->
            case GHC.Prim.<# x#_aZJ y#_aZN of _ {
              GHC.Types.False -> GHC.Prim.==# x#_aZJ y#_aZN;
              GHC.Types.True -> GHC.Types.True
            }
            }
            };
          Main.None -> GHC.Types.False
        };
      Main.None ->
        case y_a10I of _ {
          Main.Vine ipv_sZQ ipv1_sZR ipv2_sZS -> GHC.Types.True;
          Main.None -> GHC.Types.True
        }
    }

Main.$fOrdVine_$cmax :: Main.Vine -> Main.Vine -> Main.Vine
[GblId,
 Arity=2,
 Caf=NoCafRefs,
 Str=DmdType SS,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Arity=2, Value=True,
         ConLike=True, Cheap=True, Expandable=True,
         Guidance=IF_ARGS [30 30] 102 0}]
Main.$fOrdVine_$cmax =
  \ (x_a10y :: Main.Vine) (y_a10z :: Main.Vine) ->
    case x_a10y of wild_X17 {
      Main.Vine ipv_sZw ipv1_sZx ipv2_sZy ->
        case y_a10z of wild1_X1d {
          Main.Vine ipv3_sZB ipv4_sZC ipv5_sZD ->
            case ipv_sZw of _ { GHC.Types.I# x#_aZJ ->
            case ipv3_sZB of _ { GHC.Types.I# y#_aZN ->
            case GHC.Prim.<# x#_aZJ y#_aZN of _ {
              GHC.Types.False ->
                case GHC.Prim.==# x#_aZJ y#_aZN of _ {
                  GHC.Types.False -> wild_X17;
                  GHC.Types.True -> wild1_X1d
                };
              GHC.Types.True -> wild1_X1d
            }
            }
            };
          Main.None -> wild_X17
        };
      Main.None -> y_a10z
    }

Main.$fOrdVine_$cmin :: Main.Vine -> Main.Vine -> Main.Vine
[GblId,
 Arity=2,
 Caf=NoCafRefs,
 Str=DmdType SS,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Arity=2, Value=True,
         ConLike=True, Cheap=True, Expandable=True,
         Guidance=IF_ARGS [50 60] 122 30}]
Main.$fOrdVine_$cmin =
  \ (x_a10p :: Main.Vine) (y_a10q :: Main.Vine) ->
    case x_a10p of wild_X17 {
      Main.Vine ipv_sZw ipv1_sZx ipv2_sZy ->
        case y_a10q of wild1_X1d {
          Main.Vine ipv3_sZB ipv4_sZC ipv5_sZD ->
            case ipv_sZw of _ { GHC.Types.I# x#_aZJ ->
            case ipv3_sZB of _ { GHC.Types.I# y#_aZN ->
            case GHC.Prim.<# x#_aZJ y#_aZN of _ {
              GHC.Types.False ->
                case GHC.Prim.==# x#_aZJ y#_aZN of _ {
                  GHC.Types.False -> wild1_X1d;
                  GHC.Types.True -> wild_X17
                };
              GHC.Types.True -> wild_X17
            }
            }
            };
          Main.None -> Main.None
        };
      Main.None ->
        case y_a10q of _ {
          Main.Vine ipv_sZQ ipv1_sZR ipv2_sZS -> Main.None;
          Main.None -> Main.None
        }
    }

Main.$fOrdProblem_$c>1 :: Main.Vine -> Main.Vine -> GHC.Types.Bool
[GblId,
 Arity=2,
 Caf=NoCafRefs,
 Str=DmdType SS,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Arity=2, Value=True,
         ConLike=True, Cheap=True, Expandable=True,
         Guidance=IF_ARGS [50 60] 122 60}]
Main.$fOrdProblem_$c>1 =
  \ (x_a10O :: Main.Vine) (y_a10P :: Main.Vine) ->
    case x_a10O of _ {
      Main.Vine ipv_sZw ipv1_sZx ipv2_sZy ->
        case y_a10P of _ {
          Main.Vine ipv3_sZB ipv4_sZC ipv5_sZD ->
            case ipv_sZw of _ { GHC.Types.I# x#_aZJ ->
            case ipv3_sZB of _ { GHC.Types.I# y#_aZN ->
            case GHC.Prim.<# x#_aZJ y#_aZN of _ {
              GHC.Types.False ->
                case GHC.Prim.==# x#_aZJ y#_aZN of _ {
                  GHC.Types.False -> GHC.Types.True;
                  GHC.Types.True -> GHC.Types.False
                };
              GHC.Types.True -> GHC.Types.False
            }
            }
            };
          Main.None -> GHC.Types.True
        };
      Main.None ->
        case y_a10P of _ {
          Main.Vine ipv_sZQ ipv1_sZR ipv2_sZS -> GHC.Types.False;
          Main.None -> GHC.Types.False
        }
    }

Main.$fOrdProblem_$c>=1 :: Main.Vine -> Main.Vine -> GHC.Types.Bool
[GblId,
 Arity=2,
 Caf=NoCafRefs,
 Str=DmdType SS,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Arity=2, Value=True,
         ConLike=True, Cheap=True, Expandable=True,
         Guidance=IF_ARGS [50 60] 101 50}]
Main.$fOrdProblem_$c>=1 =
  \ (x_a10V :: Main.Vine) (y_a10W :: Main.Vine) ->
    case x_a10V of _ {
      Main.Vine ipv_sZw ipv1_sZx ipv2_sZy ->
        case y_a10W of _ {
          Main.Vine ipv3_sZB ipv4_sZC ipv5_sZD ->
            case ipv_sZw of _ { GHC.Types.I# x#_aZJ ->
            case ipv3_sZB of _ { GHC.Types.I# y#_aZN ->
            case GHC.Prim.<# x#_aZJ y#_aZN of _ {
              GHC.Types.False -> GHC.Types.True;
              GHC.Types.True -> GHC.Types.False
            }
            }
            };
          Main.None -> GHC.Types.True
        };
      Main.None ->
        case y_a10W of _ {
          Main.Vine ipv_sZQ ipv1_sZR ipv2_sZS -> GHC.Types.False;
          Main.None -> GHC.Types.True
        }
    }

Main.$fOrdProblem_$c<1 :: Main.Vine -> Main.Vine -> GHC.Types.Bool
[GblId,
 Arity=2,
 Caf=NoCafRefs,
 Str=DmdType SS,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Arity=2, Value=True,
         ConLike=True, Cheap=True, Expandable=True,
         Guidance=IF_ARGS [50 60] 81 30}]
Main.$fOrdProblem_$c<1 =
  \ (x_a112 :: Main.Vine) (y_a113 :: Main.Vine) ->
    case x_a112 of _ {
      Main.Vine ipv_sZw ipv1_sZx ipv2_sZy ->
        case y_a113 of _ {
          Main.Vine ipv3_sZB ipv4_sZC ipv5_sZD ->
            case ipv_sZw of _ { GHC.Types.I# x#_aZJ ->
            case ipv3_sZB of _ { GHC.Types.I# y#_aZN ->
            GHC.Prim.<# x#_aZJ y#_aZN
            }
            };
          Main.None -> GHC.Types.False
        };
      Main.None ->
        case y_a113 of _ {
          Main.Vine ipv_sZQ ipv1_sZR ipv2_sZS -> GHC.Types.True;
          Main.None -> GHC.Types.False
        }
    }

Main.$fOrdVine [InlPrag=[ALWAYS] CONLIKE]
  :: GHC.Classes.Ord Main.Vine
[GblId[DFunId],
 Caf=NoCafRefs,
 Str=DmdType m,
 Unf=DFun(arity=0) GHC.Classes.D:Ord [Main.$fEqVine,
                                      Main.$fOrdProblem_$ccompare1, Main.$fOrdProblem_$c<1,
                                      Main.$fOrdProblem_$c>=1, Main.$fOrdProblem_$c>1,
                                      Main.$fOrdProblem_$c<=1, Main.$fOrdVine_$cmax,
                                      Main.$fOrdVine_$cmin]]
Main.$fOrdVine =
  GHC.Classes.D:Ord
    @ Main.Vine
    Main.$fEqVine
    Main.$fOrdProblem_$ccompare1
    Main.$fOrdProblem_$c<1
    Main.$fOrdProblem_$c>=1
    Main.$fOrdProblem_$c>1
    Main.$fOrdProblem_$c<=1
    Main.$fOrdVine_$cmax
    Main.$fOrdVine_$cmin

Main.$fOrdProblem_$ccompare
  :: Main.Problem -> Main.Problem -> GHC.Types.Ordering
[GblId,
 Arity=2,
 Caf=NoCafRefs,
 Str=DmdType U(SA)U(SA),
 Unf=Unf{Src=InlineStable, TopLvl=True, Arity=2, Value=True,
         ConLike=True, Cheap=True, Expandable=True,
         Guidance=ALWAYS_IF(unsat_ok=True,boring_ok=False)
         Tmpl= \ (ds_dXM [Occ=Once!] :: Main.Problem)
                 (ds1_dXN [Occ=Once!] :: Main.Problem) ->
                 case ds_dXM of _ { Main.Problem v_aok [Occ=Once] _ ->
                 case ds1_dXN of _ { Main.Problem v'_aol [Occ=Once] _ ->
                 Main.$fOrdProblem_$ccompare1 v_aok v'_aol
                 }
                 }}]
Main.$fOrdProblem_$ccompare =
  \ (ds_dXM :: Main.Problem) (ds1_dXN :: Main.Problem) ->
    case ds_dXM of _ { Main.Problem v_aok ds2_dXO ->
    case ds1_dXN of _ { Main.Problem v'_aol ds3_dXP ->
    Main.$fOrdProblem_$ccompare1 v_aok v'_aol
    }
    }

Main.$fOrdProblem_$c<= [InlPrag=INLINE[0]]
  :: Main.Problem -> Main.Problem -> GHC.Types.Bool
[GblId,
 Arity=2,
 Caf=NoCafRefs,
 Str=DmdType U(SA)U(SA),
 Unf=Unf{Src=InlineStable, TopLvl=True, Arity=2, Value=True,
         ConLike=True, Cheap=True, Expandable=True,
         Guidance=ALWAYS_IF(unsat_ok=True,boring_ok=False)
         Tmpl= \ (w_s36g [Occ=Once!] :: Main.Problem)
                 (w1_s36l [Occ=Once!] :: Main.Problem) ->
                 case w_s36g of _ { Main.Problem ww_s36i [Occ=Once] _ ->
                 case w1_s36l of _ { Main.Problem ww2_s36n [Occ=Once] _ ->
                 Main.$fOrdProblem_$c<=1 ww_s36i ww2_s36n
                 }
                 }}]
Main.$fOrdProblem_$c<= =
  \ (w_s36g :: Main.Problem) (w1_s36l :: Main.Problem) ->
    case w_s36g of _ { Main.Problem ww_s36i ww1_s36j ->
    case w1_s36l of _ { Main.Problem ww2_s36n ww3_s36o ->
    Main.$fOrdProblem_$c<=1 ww_s36i ww2_s36n
    }
    }

Main.$fOrdProblem_$cmax [InlPrag=INLINE[0]]
  :: Main.Problem -> Main.Problem -> Main.Problem
[GblId,
 Arity=2,
 Caf=NoCafRefs,
 Str=DmdType U(SL)U(SL)m,
 Unf=Unf{Src=InlineStable, TopLvl=True, Arity=2, Value=True,
         ConLike=True, Cheap=True, Expandable=True,
         Guidance=ALWAYS_IF(unsat_ok=True,boring_ok=False)
         Tmpl= \ (w_s36t [Occ=Once!] :: Main.Problem)
                 (w1_s36y [Occ=Once!] :: Main.Problem) ->
                 case w_s36t
                 of _ { Main.Problem ww_s36v [Occ=Once!] ww1_s36w [Occ=Once*] ->
                 case w1_s36y
                 of _ { Main.Problem ww2_s36A [Occ=Once*!] ww3_s36B [Occ=Once*] ->
                 case ww_s36v of wild_X17 {
                   Main.Vine ipv_sZw [Occ=Once!] _ _ ->
                     case ww2_s36A of wild1_X1d {
                       Main.Vine ipv3_sZB [Occ=Once!] _ _ ->
                         case ipv_sZw of _ { GHC.Types.I# x#_aZJ ->
                         case ipv3_sZB of _ { GHC.Types.I# y#_aZN ->
                         case GHC.Prim.<# x#_aZJ y#_aZN of _ {
                           GHC.Types.False ->
                             case GHC.Prim.==# x#_aZJ y#_aZN of _ {
                               GHC.Types.False -> Main.Problem wild_X17 ww1_s36w;
                               GHC.Types.True -> Main.Problem wild1_X1d ww3_s36B
                             };
                           GHC.Types.True -> Main.Problem wild1_X1d ww3_s36B
                         }
                         }
                         };
                       Main.None -> Main.Problem wild_X17 ww1_s36w
                     };
                   Main.None ->
                     case ww2_s36A of wild1_Xl {
                       Main.Vine _ _ _ -> Main.Problem wild1_Xl ww3_s36B;
                       Main.None -> Main.Problem Main.None ww3_s36B
                     }
                 }
                 }
                 }}]
Main.$fOrdProblem_$cmax =
  \ (w_s36t :: Main.Problem) (w1_s36y :: Main.Problem) ->
    case w_s36t of _ { Main.Problem ww_s36v ww1_s36w ->
    case w1_s36y of _ { Main.Problem ww2_s36A ww3_s36B ->
    case ww_s36v of wild_X17 {
      Main.Vine ipv_sZw ipv1_sZx ipv2_sZy ->
        case ww2_s36A of wild1_X1d {
          Main.Vine ipv3_sZB ipv4_sZC ipv5_sZD ->
            case ipv_sZw of _ { GHC.Types.I# x#_aZJ ->
            case ipv3_sZB of _ { GHC.Types.I# y#_aZN ->
            case GHC.Prim.<# x#_aZJ y#_aZN of _ {
              GHC.Types.False ->
                case GHC.Prim.==# x#_aZJ y#_aZN of _ {
                  GHC.Types.False -> Main.Problem wild_X17 ww1_s36w;
                  GHC.Types.True -> Main.Problem wild1_X1d ww3_s36B
                };
              GHC.Types.True -> Main.Problem wild1_X1d ww3_s36B
            }
            }
            };
          Main.None -> Main.Problem wild_X17 ww1_s36w
        };
      Main.None ->
        case ww2_s36A of wild1_Xl {
          Main.Vine ipv_sZQ ipv1_sZR ipv2_sZS ->
            Main.Problem wild1_Xl ww3_s36B;
          Main.None -> Main.Problem Main.None ww3_s36B
        }
    }
    }
    }

Main.$fOrdProblem_$cmin [InlPrag=INLINE[0]]
  :: Main.Problem -> Main.Problem -> Main.Problem
[GblId,
 Arity=2,
 Caf=NoCafRefs,
 Str=DmdType U(SL)U(SL)m,
 Unf=Unf{Src=InlineStable, TopLvl=True, Arity=2, Value=True,
         ConLike=True, Cheap=True, Expandable=True,
         Guidance=ALWAYS_IF(unsat_ok=True,boring_ok=False)
         Tmpl= \ (w_s36H [Occ=Once!] :: Main.Problem)
                 (w1_s36M [Occ=Once!] :: Main.Problem) ->
                 case w_s36H
                 of _ { Main.Problem ww_s36J [Occ=Once!] ww1_s36K [Occ=Once*] ->
                 case w1_s36M
                 of _ { Main.Problem ww2_s36O [Occ=Once*!] ww3_s36P [Occ=Once*] ->
                 case ww_s36J of wild_X17 {
                   Main.Vine ipv_sZw [Occ=Once!] _ _ ->
                     case ww2_s36O of wild1_X1d {
                       Main.Vine ipv3_sZB [Occ=Once!] _ _ ->
                         case ipv_sZw of _ { GHC.Types.I# x#_aZJ ->
                         case ipv3_sZB of _ { GHC.Types.I# y#_aZN ->
                         case GHC.Prim.<# x#_aZJ y#_aZN of _ {
                           GHC.Types.False ->
                             case GHC.Prim.==# x#_aZJ y#_aZN of _ {
                               GHC.Types.False -> Main.Problem wild1_X1d ww3_s36P;
                               GHC.Types.True -> Main.Problem wild_X17 ww1_s36K
                             };
                           GHC.Types.True -> Main.Problem wild_X17 ww1_s36K
                         }
                         }
                         };
                       Main.None -> Main.Problem Main.None ww3_s36P
                     };
                   Main.None ->
                     case ww2_s36O of _ {
                       Main.Vine _ _ _ -> Main.Problem Main.None ww1_s36K;
                       Main.None -> Main.Problem Main.None ww1_s36K
                     }
                 }
                 }
                 }}]
Main.$fOrdProblem_$cmin =
  \ (w_s36H :: Main.Problem) (w1_s36M :: Main.Problem) ->
    case w_s36H of _ { Main.Problem ww_s36J ww1_s36K ->
    case w1_s36M of _ { Main.Problem ww2_s36O ww3_s36P ->
    case ww_s36J of wild_X17 {
      Main.Vine ipv_sZw ipv1_sZx ipv2_sZy ->
        case ww2_s36O of wild1_X1d {
          Main.Vine ipv3_sZB ipv4_sZC ipv5_sZD ->
            case ipv_sZw of _ { GHC.Types.I# x#_aZJ ->
            case ipv3_sZB of _ { GHC.Types.I# y#_aZN ->
            case GHC.Prim.<# x#_aZJ y#_aZN of _ {
              GHC.Types.False ->
                case GHC.Prim.==# x#_aZJ y#_aZN of _ {
                  GHC.Types.False -> Main.Problem wild1_X1d ww3_s36P;
                  GHC.Types.True -> Main.Problem wild_X17 ww1_s36K
                };
              GHC.Types.True -> Main.Problem wild_X17 ww1_s36K
            }
            }
            };
          Main.None -> Main.Problem Main.None ww3_s36P
        };
      Main.None ->
        case ww2_s36O of _ {
          Main.Vine ipv_sZQ ipv1_sZR ipv2_sZS ->
            Main.Problem Main.None ww1_s36K;
          Main.None -> Main.Problem Main.None ww1_s36K
        }
    }
    }
    }

Main.$fOrdProblem_$c> [InlPrag=INLINE[0]]
  :: Main.Problem -> Main.Problem -> GHC.Types.Bool
[GblId,
 Arity=2,
 Caf=NoCafRefs,
 Str=DmdType U(SA)U(SA),
 Unf=Unf{Src=InlineStable, TopLvl=True, Arity=2, Value=True,
         ConLike=True, Cheap=True, Expandable=True,
         Guidance=ALWAYS_IF(unsat_ok=True,boring_ok=False)
         Tmpl= \ (w_s36V [Occ=Once!] :: Main.Problem)
                 (w1_s370 [Occ=Once!] :: Main.Problem) ->
                 case w_s36V of _ { Main.Problem ww_s36X [Occ=Once] _ ->
                 case w1_s370 of _ { Main.Problem ww2_s372 [Occ=Once] _ ->
                 Main.$fOrdProblem_$c>1 ww_s36X ww2_s372
                 }
                 }}]
Main.$fOrdProblem_$c> =
  \ (w_s36V :: Main.Problem) (w1_s370 :: Main.Problem) ->
    case w_s36V of _ { Main.Problem ww_s36X ww1_s36Y ->
    case w1_s370 of _ { Main.Problem ww2_s372 ww3_s373 ->
    Main.$fOrdProblem_$c>1 ww_s36X ww2_s372
    }
    }

Main.$fOrdProblem_$c>= [InlPrag=INLINE[0]]
  :: Main.Problem -> Main.Problem -> GHC.Types.Bool
[GblId,
 Arity=2,
 Caf=NoCafRefs,
 Str=DmdType U(SA)U(SA),
 Unf=Unf{Src=InlineStable, TopLvl=True, Arity=2, Value=True,
         ConLike=True, Cheap=True, Expandable=True,
         Guidance=ALWAYS_IF(unsat_ok=True,boring_ok=False)
         Tmpl= \ (w_s378 [Occ=Once!] :: Main.Problem)
                 (w1_s37d [Occ=Once!] :: Main.Problem) ->
                 case w_s378 of _ { Main.Problem ww_s37a [Occ=Once] _ ->
                 case w1_s37d of _ { Main.Problem ww2_s37f [Occ=Once] _ ->
                 Main.$fOrdProblem_$c>=1 ww_s37a ww2_s37f
                 }
                 }}]
Main.$fOrdProblem_$c>= =
  \ (w_s378 :: Main.Problem) (w1_s37d :: Main.Problem) ->
    case w_s378 of _ { Main.Problem ww_s37a ww1_s37b ->
    case w1_s37d of _ { Main.Problem ww2_s37f ww3_s37g ->
    Main.$fOrdProblem_$c>=1 ww_s37a ww2_s37f
    }
    }

Main.$fOrdProblem_$c< [InlPrag=INLINE[0]]
  :: Main.Problem -> Main.Problem -> GHC.Types.Bool
[GblId,
 Arity=2,
 Caf=NoCafRefs,
 Str=DmdType U(SA)U(SA),
 Unf=Unf{Src=InlineStable, TopLvl=True, Arity=2, Value=True,
         ConLike=True, Cheap=True, Expandable=True,
         Guidance=ALWAYS_IF(unsat_ok=True,boring_ok=False)
         Tmpl= \ (w_s37l [Occ=Once!] :: Main.Problem)
                 (w1_s37q [Occ=Once!] :: Main.Problem) ->
                 case w_s37l of _ { Main.Problem ww_s37n [Occ=Once] _ ->
                 case w1_s37q of _ { Main.Problem ww2_s37s [Occ=Once] _ ->
                 Main.$fOrdProblem_$c<1 ww_s37n ww2_s37s
                 }
                 }}]
Main.$fOrdProblem_$c< =
  \ (w_s37l :: Main.Problem) (w1_s37q :: Main.Problem) ->
    case w_s37l of _ { Main.Problem ww_s37n ww1_s37o ->
    case w1_s37q of _ { Main.Problem ww2_s37s ww3_s37t ->
    Main.$fOrdProblem_$c<1 ww_s37n ww2_s37s
    }
    }

Main.$fOrdProblem [InlPrag=[ALWAYS] CONLIKE]
  :: GHC.Classes.Ord Main.Problem
[GblId[DFunId],
 Caf=NoCafRefs,
 Str=DmdType m,
 Unf=DFun(arity=0) GHC.Classes.D:Ord [Main.$fEqProblem,
                                      Main.$fOrdProblem_$ccompare, Main.$fOrdProblem_$c<,
                                      Main.$fOrdProblem_$c>=, Main.$fOrdProblem_$c>,
                                      Main.$fOrdProblem_$c<=, Main.$fOrdProblem_$cmax,
                                      Main.$fOrdProblem_$cmin]]
Main.$fOrdProblem =
  GHC.Classes.D:Ord
    @ Main.Problem
    Main.$fEqProblem
    Main.$fOrdProblem_$ccompare
    Main.$fOrdProblem_$c<
    Main.$fOrdProblem_$c>=
    Main.$fOrdProblem_$c>
    Main.$fOrdProblem_$c<=
    Main.$fOrdProblem_$cmax
    Main.$fOrdProblem_$cmin

Rec {
$wlgo_r3cD :: GHC.Prim.Int# -> [GHC.Types.Int] -> GHC.Prim.Int#
[GblId, Arity=2, Caf=NoCafRefs, Str=DmdType LS]
$wlgo_r3cD =
  \ (ww_s37A :: GHC.Prim.Int#) (w_s37C :: [GHC.Types.Int]) ->
    case w_s37C of _ {
      [] -> ww_s37A;
      : x_a1eC xs_a1eD ->
        case x_a1eC of _ { GHC.Types.I# y1_a1eO ->
        case GHC.Prim.<=# ww_s37A y1_a1eO of _ {
          GHC.Types.False -> $wlgo_r3cD ww_s37A xs_a1eD;
          GHC.Types.True -> $wlgo_r3cD y1_a1eO xs_a1eD
        }
        }
    }
end Rec }

Rec {
$s$wsolve_r3cE
  :: GHC.Prim.Int#
     -> GHC.Types.Int -> Main.Vine -> GHC.Prim.Int# -> GHC.Types.Bool
[GblId, Arity=4, Str=DmdType LLLL]
$s$wsolve_r3cE =
  \ (sc_s3a6 :: GHC.Prim.Int#)
    (sc1_s3a7 :: GHC.Types.Int)
    (sc2_s3a8 :: Main.Vine)
    (sc3_s3a9 :: GHC.Prim.Int#) ->
    case sc1_s3a7 of wild1_a11i { GHC.Types.I# y_a11k ->
    case GHC.Prim.>=# (GHC.Prim.+# sc_s3a6 y_a11k) sc3_s3a9 of _ {
      GHC.Types.False ->
        case GHC.Base.map
               @ Main.Vine
               @ GHC.Types.Int
               Main.position
               (asList_r3cB
                  (Main.Vine (GHC.Types.I# sc_s3a6) wild1_a11i sc2_s3a8))
        of _ {
          [] ->
            Data.List.maximum1
            `cast` (UnsafeCo (forall a_a11J. a_a11J) GHC.Types.Bool
                    :: (forall a_a11J. a_a11J) ~# GHC.Types.Bool);
          : ipv_a11N ipv1_a11O ->
            case ipv_a11N of _ { GHC.Types.I# ww_s37A ->
            case $wlgo_r3cD ww_s37A ipv1_a11O of ww1_s37F { __DEFAULT ->
            case GHC.Prim.<# (GHC.Prim.+# y_a11k ww1_s37F) sc3_s3a9 of _ {
              GHC.Types.False ->
                case asList_r3cB
                       (Main.Vine (GHC.Types.I# sc_s3a6) wild1_a11i sc2_s3a8)
                of _ {
                  [] ->
                    GHC.List.tail1
                    `cast` (UnsafeCo (forall a_a135. [a_a135]) GHC.Types.Bool
                            :: (forall a_a135. [a_a135]) ~# GHC.Types.Bool);
                  : _ xs_a138 ->
                    case GHC.List.reverse1
                           @ Main.Vine
                           (Data.List.sortBy
                              @ Main.Vine
                              Main.$fOrdProblem_$ccompare1
                              (GHC.List.filter
                                 @ Main.Vine
                                 (\ (v'_agY :: Main.Vine) ->
                                    case v'_agY of _ {
                                      Main.Vine ds_dXh ds2_dXi ds3_dXj ->
                                        case ds_dXh of _ { GHC.Types.I# y1_X13j ->
                                        GHC.Prim.>=# (GHC.Prim.+# sc_s3a6 y_a11k) y1_X13j
                                        };
                                      Main.None ->
                                        Main.position1
                                        `cast` (UnsafeCo GHC.Types.Int GHC.Types.Bool
                                                :: GHC.Types.Int ~# GHC.Types.Bool)
                                    })
                                 xs_a138))
                           (GHC.Types.[] @ Main.Vine)
                    of _ {
                      [] -> GHC.Types.False;
                      : y1_a12k ys_a12l ->
                        case y1_a12k of _ {
                          Main.Vine ds_dXh ds2_dXi ds3_dXj ->
                            case ds_dXh of _ { GHC.Types.I# y2_X16d ->
                            case GHC.Prim.<# (GHC.Prim.+# sc_s3a6 y_a11k) y2_X16d of _ {
                              GHC.Types.False ->
                                case $s$wsolve_r3cE
                                       y2_X16d
                                       (case ds2_dXi of wild9_a12D { GHC.Types.I# y3_a12F ->
                                        let {
                                          x1_a12B [Dmd=Just L] :: GHC.Prim.Int#
                                          [LclId, Str=DmdType]
                                          x1_a12B = GHC.Prim.-# y2_X16d sc_s3a6 } in
                                        case GHC.Prim.<=# x1_a12B y3_a12F of _ {
                                          GHC.Types.False -> wild9_a12D;
                                          GHC.Types.True -> GHC.Types.I# x1_a12B
                                        }
                                        })
                                       ds3_dXj
                                       sc3_s3a9
                                of _ {
                                  GHC.Types.False ->
                                    letrec {
                                      go_a12e [Occ=LoopBreaker] :: [Main.Vine] -> GHC.Types.Bool
                                      [LclId, Arity=1, Str=DmdType S]
                                      go_a12e =
                                        \ (ds4_a12f :: [Main.Vine]) ->
                                          case ds4_a12f of _ {
                                            [] -> GHC.Types.False;
                                            : y3_X15r ys1_X15t ->
                                              case y3_X15r of _ {
                                                Main.Vine ds5_X10u ds6_X10w ds7_X10y ->
                                                  case ds5_X10u of _ { GHC.Types.I# y4_X19w ->
                                                  case GHC.Prim.<#
                                                         (GHC.Prim.+# sc_s3a6 y_a11k) y4_X19w
                                                  of _ {
                                                    GHC.Types.False ->
                                                      case $s$wsolve_r3cE
                                                             y4_X19w
                                                             (case ds6_X10w
                                                              of wild14_a12D
                                                              { GHC.Types.I# y5_a12F ->
                                                              let {
                                                                x1_a12B [Dmd=Just L]
                                                                  :: GHC.Prim.Int#
                                                                [LclId, Str=DmdType]
                                                                x1_a12B =
                                                                  GHC.Prim.-# y4_X19w sc_s3a6 } in
                                                              case GHC.Prim.<=# x1_a12B y5_a12F
                                                              of _ {
                                                                GHC.Types.False -> wild14_a12D;
                                                                GHC.Types.True ->
                                                                  GHC.Types.I# x1_a12B
                                                              }
                                                              })
                                                             ds7_X10y
                                                             sc3_s3a9
                                                      of _ {
                                                        GHC.Types.False -> go_a12e ys1_X15t;
                                                        GHC.Types.True -> GHC.Types.True
                                                      };
                                                    GHC.Types.True ->
                                                      case GHC.Prim.>=# 0 sc3_s3a9 of _ {
                                                        GHC.Types.False ->
                                                          Main.height1
                                                          `cast` (UnsafeCo
                                                                    GHC.Types.Int GHC.Types.Bool
                                                                  :: GHC.Types.Int
                                                                       ~#
                                                                     GHC.Types.Bool);
                                                        GHC.Types.True -> GHC.Types.True
                                                      }
                                                  }
                                                  };
                                                Main.None ->
                                                  Main.position1
                                                  `cast` (UnsafeCo GHC.Types.Int GHC.Types.Bool
                                                          :: GHC.Types.Int ~# GHC.Types.Bool)
                                              }
                                          }; } in
                                    go_a12e ys_a12l;
                                  GHC.Types.True -> GHC.Types.True
                                };
                              GHC.Types.True ->
                                case GHC.Prim.>=# 0 sc3_s3a9 of _ {
                                  GHC.Types.False ->
                                    Main.height1
                                    `cast` (UnsafeCo GHC.Types.Int GHC.Types.Bool
                                            :: GHC.Types.Int ~# GHC.Types.Bool);
                                  GHC.Types.True -> GHC.Types.True
                                }
                            }
                            };
                          Main.None ->
                            Main.position1
                            `cast` (UnsafeCo GHC.Types.Int GHC.Types.Bool
                                    :: GHC.Types.Int ~# GHC.Types.Bool)
                        }
                    }
                };
              GHC.Types.True -> GHC.Types.False
            }
            }
            }
        };
      GHC.Types.True -> GHC.Types.True
    }
    }
end Rec }

Main.main4 :: (GHC.Types.Int, [GHC.Types.Int])
[GblId, Str=DmdType b]
Main.main4 =
  Control.Exception.Base.irrefutPatError
    @ (GHC.Types.Int, [GHC.Types.Int])
    "main.hs:31:11-67|(problemCount : numbers)"

lvl1_r3cF :: Main.Vine
[GblId, Str=DmdType b]
lvl1_r3cF =
  Control.Exception.Base.patError
    @ Main.Vine "main.hs:(42,1)-(44,49)|function readVine"

lvl2_r3cG :: [Main.Problem]
[GblId, Str=DmdType b]
lvl2_r3cG =
  Control.Exception.Base.patError
    @ [Main.Problem] "main.hs:(33,1)-(40,27)|function buildProblems"

Rec {
$wreadVine_r3cH :: GHC.Prim.Int# -> [GHC.Types.Int] -> Main.Vine
[GblId, Arity=2, Str=DmdType LL]
$wreadVine_r3cH =
  \ (ww_s37V :: GHC.Prim.Int#) (w_s37X :: [GHC.Types.Int]) ->
    case ww_s37V of wild_X3B {
      __DEFAULT ->
        case w_s37X of _ {
          [] -> lvl1_r3cF;
          : p_anO ds_dWh ->
            case ds_dWh of _ {
              [] -> lvl1_r3cF;
              : h_anP numbers_anQ ->
                Main.Vine
                  p_anO h_anP ($wreadVine_r3cH (GHC.Prim.-# wild_X3B 1) numbers_anQ)
            }
        };
      0 -> Main.None
    }
end Rec }

Rec {
Main.$wbuildProblems [Occ=LoopBreaker]
  :: GHC.Prim.Int# -> [GHC.Types.Int] -> [Main.Problem]
[GblId, Arity=2, Str=DmdType LL]
Main.$wbuildProblems =
  \ (ww_s383 :: GHC.Prim.Int#) (w_s385 :: [GHC.Types.Int]) ->
    case ww_s383 of wild_X3C {
      __DEFAULT ->
        case w_s385 of _ {
          [] -> lvl2_r3cG;
          : vineCount_an7 numbers_an8 ->
            let {
              x_s1bi [Dmd=Just D(L)] :: GHC.Types.Int
              [LclId, Str=DmdType]
              x_s1bi =
                case vineCount_an7 of _ { GHC.Types.I# y_a16t ->
                GHC.Types.I# (GHC.Prim.*# 2 y_a16t)
                } } in
            GHC.Types.:
              @ Main.Problem
              (let {
                 vine_s1bh [Dmd=Just L] :: Main.Vine
                 [LclId, Str=DmdType]
                 vine_s1bh =
                   case vineCount_an7 of _ { GHC.Types.I# ww1_s37V ->
                   $wreadVine_r3cH ww1_s37V numbers_an8
                   } } in
               Main.Problem
                 (Main.Vine
                    lvl_r3cC
                    (case vine_s1bh of _ {
                       Main.Vine ds_dXh ds1_dXi ds2_dXj -> ds_dXh;
                       Main.None -> Main.position1
                     })
                    vine_s1bh)
                 (GHC.List.!! @ GHC.Types.Int numbers_an8 x_s1bi))
              (Main.$wbuildProblems
                 (GHC.Prim.-# wild_X3C 1)
                 (case x_s1bi of _ { GHC.Types.I# x1_a11g ->
                  let {
                    n#_a16R [Dmd=Just L] :: GHC.Prim.Int#
                    [LclId, Str=DmdType]
                    n#_a16R = GHC.Prim.+# x1_a11g 1 } in
                  case GHC.Prim.<# n#_a16R 0 of _ {
                    GHC.Types.False ->
                      GHC.List.drop_drop# @ GHC.Types.Int n#_a16R numbers_an8;
                    GHC.Types.True -> numbers_an8
                  }
                  }))
        };
      0 -> GHC.Types.[] @ Main.Problem
    }
end Rec }

Main.main3 :: GHC.Integer.Type.Integer
[GblId,
 Str=DmdType,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Arity=0, Value=True,
         ConLike=True, Cheap=True, Expandable=True,
         Guidance=IF_ARGS [] 100 0}]
Main.main3 = __integer 1

lvl3_r3cI :: [GHC.Types.Char]
[GblId, Str=DmdType]
lvl3_r3cI = GHC.CString.unpackCString# "YES"

lvl4_r3cJ :: [GHC.Types.Char]
[GblId, Str=DmdType]
lvl4_r3cJ = GHC.CString.unpackCString# "NO"

lvl5_r3cK :: GHC.Types.Char
[GblId, Caf=NoCafRefs, Str=DmdType m]
lvl5_r3cK = GHC.Types.C# '\n'

Rec {
Main.main_showProblem [Occ=LoopBreaker]
  :: GHC.Integer.Type.Integer -> [Main.Problem] -> [GHC.Types.Char]
[GblId, Arity=2, Str=DmdType LS]
Main.main_showProblem =
  \ (ds_dX6 :: GHC.Integer.Type.Integer)
    (ds1_dX7 :: [Main.Problem]) ->
    case ds1_dX7 of _ {
      [] -> GHC.Types.[] @ GHC.Types.Char;
      : p_ao6 problems_ao7 ->
        GHC.CString.unpackAppendCString#
          "Case #"
          (GHC.Base.++
             @ GHC.Types.Char
             (GHC.Show.$w$cshowsPrec 0 ds_dX6 (GHC.Types.[] @ GHC.Types.Char))
             (GHC.CString.unpackAppendCString#
                ": "
                (case p_ao6 of _ { Main.Problem ww_s37K ww1_s37L ->
                 case ww1_s37L of _ { GHC.Types.I# ww3_s37N ->
                 let {
                   $j_s1fl :: GHC.Prim.Int# -> GHC.Types.Bool
                   [LclId, Arity=1, Str=DmdType L]
                   $j_s1fl =
                     \ (x_aZ5 :: GHC.Prim.Int#) ->
                       case GHC.Prim.>=# x_aZ5 ww3_s37N of _ {
                         GHC.Types.False ->
                           case ww_s37K of wild2_XT {
                             Main.Vine ds2_dXo ds3_dXp ds4_dXq ->
                               case ds3_dXp of _ { GHC.Types.I# x1_a11g ->
                               case GHC.Base.map
                                      @ Main.Vine
                                      @ GHC.Types.Int
                                      Main.position
                                      (asList_r3cB wild2_XT)
                               of _ {
                                 [] ->
                                   Data.List.maximum1
                                   `cast` (UnsafeCo (forall a_a11J. a_a11J) GHC.Types.Bool
                                           :: (forall a_a11J. a_a11J) ~# GHC.Types.Bool);
                                 : ipv_a11N ipv1_a11O ->
                                   case ipv_a11N of _ { GHC.Types.I# ww4_s37A ->
                                   case $wlgo_r3cD ww4_s37A ipv1_a11O of ww5_s37F { __DEFAULT ->
                                   case GHC.Prim.<# (GHC.Prim.+# x1_a11g ww5_s37F) ww3_s37N of _ {
                                     GHC.Types.False ->
                                       case asList_r3cB wild2_XT of _ {
                                         [] ->
                                           GHC.List.tail1
                                           `cast` (UnsafeCo (forall a_a135. [a_a135]) GHC.Types.Bool
                                                   :: (forall a_a135. [a_a135]) ~# GHC.Types.Bool);
                                         : _ xs_a138 ->
                                           case GHC.List.reverse1
                                                  @ Main.Vine
                                                  (Data.List.sortBy
                                                     @ Main.Vine
                                                     Main.$fOrdProblem_$ccompare1
                                                     (GHC.List.filter
                                                        @ Main.Vine
                                                        (\ (v'_agY :: Main.Vine) ->
                                                           case ds2_dXo
                                                           of _ { GHC.Types.I# x2_X15g ->
                                                           case v'_agY of _ {
                                                             Main.Vine ds6_dXh ds7_dXi ds8_dXj ->
                                                               case ds6_dXh
                                                               of _ { GHC.Types.I# y_X13j ->
                                                               GHC.Prim.>=#
                                                                 (GHC.Prim.+# x2_X15g x1_a11g)
                                                                 y_X13j
                                                               };
                                                             Main.None ->
                                                               Main.position1
                                                               `cast` (UnsafeCo
                                                                         GHC.Types.Int
                                                                         GHC.Types.Bool
                                                                       :: GHC.Types.Int
                                                                            ~#
                                                                          GHC.Types.Bool)
                                                           }
                                                           })
                                                        xs_a138))
                                                  (GHC.Types.[] @ Main.Vine)
                                           of _ {
                                             [] -> GHC.Types.False;
                                             : y_a12k ys_a12l ->
                                               case ds2_dXo of _ { GHC.Types.I# x2_X15d ->
                                               case y_a12k of _ {
                                                 Main.Vine ds6_dXh ds7_dXi ds8_dXj ->
                                                   case ds6_dXh of _ { GHC.Types.I# y1_X16d ->
                                                   case GHC.Prim.<#
                                                          (GHC.Prim.+# x2_X15d x1_a11g) y1_X16d
                                                   of _ {
                                                     GHC.Types.False ->
                                                       case $s$wsolve_r3cE
                                                              y1_X16d
                                                              (case ds7_dXi
                                                               of wild12_a12D
                                                               { GHC.Types.I# y2_a12F ->
                                                               let {
                                                                 x3_a12B [Dmd=Just L]
                                                                   :: GHC.Prim.Int#
                                                                 [LclId, Str=DmdType]
                                                                 x3_a12B =
                                                                   GHC.Prim.-# y1_X16d x2_X15d } in
                                                               case GHC.Prim.<=# x3_a12B y2_a12F
                                                               of _ {
                                                                 GHC.Types.False -> wild12_a12D;
                                                                 GHC.Types.True ->
                                                                   GHC.Types.I# x3_a12B
                                                               }
                                                               })
                                                              ds8_dXj
                                                              ww3_s37N
                                                       of _ {
                                                         GHC.Types.False ->
                                                           letrec {
                                                             go_a12e [Occ=LoopBreaker]
                                                               :: [Main.Vine] -> GHC.Types.Bool
                                                             [LclId, Arity=1, Str=DmdType S]
                                                             go_a12e =
                                                               \ (ds9_a12f :: [Main.Vine]) ->
                                                                 case ds9_a12f of _ {
                                                                   [] -> GHC.Types.False;
                                                                   : y2_X15r ys1_X15t ->
                                                                     case y2_X15r of _ {
                                                                       Main.Vine ds10_X10u
                                                                                 ds11_X10w
                                                                                 ds12_X10y ->
                                                                         case ds10_X10u
                                                                         of _
                                                                         { GHC.Types.I# y3_X19w ->
                                                                         case GHC.Prim.<#
                                                                                (GHC.Prim.+#
                                                                                   x2_X15d x1_a11g)
                                                                                y3_X19w
                                                                         of _ {
                                                                           GHC.Types.False ->
                                                                             case $s$wsolve_r3cE
                                                                                    y3_X19w
                                                                                    (case ds11_X10w
                                                                                     of wild17_a12D
                                                                                     { GHC.Types.I# y4_a12F ->
                                                                                     let {
                                                                                       x3_a12B [Dmd=Just L]
                                                                                         :: GHC.Prim.Int#
                                                                                       [LclId,
                                                                                        Str=DmdType]
                                                                                       x3_a12B =
                                                                                         GHC.Prim.-#
                                                                                           y3_X19w
                                                                                           x2_X15d } in
                                                                                     case GHC.Prim.<=#
                                                                                            x3_a12B
                                                                                            y4_a12F
                                                                                     of _ {
                                                                                       GHC.Types.False ->
                                                                                         wild17_a12D;
                                                                                       GHC.Types.True ->
                                                                                         GHC.Types.I#
                                                                                           x3_a12B
                                                                                     }
                                                                                     })
                                                                                    ds12_X10y
                                                                                    ww3_s37N
                                                                             of _ {
                                                                               GHC.Types.False ->
                                                                                 go_a12e ys1_X15t;
                                                                               GHC.Types.True ->
                                                                                 GHC.Types.True
                                                                             };
                                                                           GHC.Types.True ->
                                                                             case GHC.Prim.>=#
                                                                                    0 ww3_s37N
                                                                             of _ {
                                                                               GHC.Types.False ->
                                                                                 Main.height1
                                                                                 `cast` (UnsafeCo
                                                                                           GHC.Types.Int
                                                                                           GHC.Types.Bool
                                                                                         :: GHC.Types.Int
                                                                                              ~#
                                                                                            GHC.Types.Bool);
                                                                               GHC.Types.True ->
                                                                                 GHC.Types.True
                                                                             }
                                                                         }
                                                                         };
                                                                       Main.None ->
                                                                         Main.position1
                                                                         `cast` (UnsafeCo
                                                                                   GHC.Types.Int
                                                                                   GHC.Types.Bool
                                                                                 :: GHC.Types.Int
                                                                                      ~#
                                                                                    GHC.Types.Bool)
                                                                     }
                                                                 }; } in
                                                           go_a12e ys_a12l;
                                                         GHC.Types.True -> GHC.Types.True
                                                       };
                                                     GHC.Types.True ->
                                                       case GHC.Prim.>=# 0 ww3_s37N of _ {
                                                         GHC.Types.False ->
                                                           Main.height1
                                                           `cast` (UnsafeCo
                                                                     GHC.Types.Int GHC.Types.Bool
                                                                   :: GHC.Types.Int
                                                                        ~#
                                                                      GHC.Types.Bool);
                                                         GHC.Types.True -> GHC.Types.True
                                                       }
                                                   }
                                                   };
                                                 Main.None ->
                                                   Main.position1
                                                   `cast` (UnsafeCo GHC.Types.Int GHC.Types.Bool
                                                           :: GHC.Types.Int ~# GHC.Types.Bool)
                                               }
                                               }
                                           }
                                       };
                                     GHC.Types.True -> GHC.Types.False
                                   }
                                   }
                                   }
                               }
                               };
                             Main.None ->
                               Main.height1
                               `cast` (UnsafeCo GHC.Types.Int GHC.Types.Bool
                                       :: GHC.Types.Int ~# GHC.Types.Bool)
                           };
                         GHC.Types.True -> GHC.Types.True
                       } } in
                 let {
                   $j1_s3cv :: GHC.Prim.State# GHC.Prim.RealWorld -> [GHC.Types.Char]
                   [LclId, Arity=1]
                   $j1_s3cv =
                     \ _ ->
                       GHC.Base.++
                         @ GHC.Types.Char
                         lvl4_r3cJ
                         (GHC.Types.:
                            @ GHC.Types.Char
                            lvl5_r3cK
                            (Main.main_showProblem
                               (GHC.Integer.Type.plusInteger ds_dX6 Main.main3)
                               problems_ao7)) } in
                 let {
                   $j2_s3cz :: GHC.Prim.State# GHC.Prim.RealWorld -> [GHC.Types.Char]
                   [LclId, Arity=1]
                   $j2_s3cz =
                     \ _ ->
                       GHC.Base.++
                         @ GHC.Types.Char
                         lvl3_r3cI
                         (GHC.Types.:
                            @ GHC.Types.Char
                            lvl5_r3cK
                            (Main.main_showProblem
                               (GHC.Integer.Type.plusInteger ds_dX6 Main.main3)
                               problems_ao7)) } in
                 case ww_s37K of _ {
                   Main.Vine ipv_s118 ipv1_s119 ipv2_s11a ->
                     case ipv_s118 of _ { GHC.Types.I# x_a11g ->
                     case ipv1_s119 of _ { GHC.Types.I# y_a11k ->
                     case $j_s1fl (GHC.Prim.+# x_a11g y_a11k) of _ {
                       GHC.Types.False -> $j1_s3cv GHC.Prim.realWorld#;
                       GHC.Types.True -> $j2_s3cz GHC.Prim.realWorld#
                     }
                     }
                     };
                   Main.None ->
                     case $j_s1fl 0 of _ {
                       GHC.Types.False -> $j1_s3cv GHC.Prim.realWorld#;
                       GHC.Types.True -> $j2_s3cz GHC.Prim.realWorld#
                     }
                 }
                 }
                 })))
    }
end Rec }

Main.main5 :: GHC.Base.String -> GHC.Types.Int
[GblId,
 Arity=1,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Arity=1, Value=True,
         ConLike=True, Cheap=True, Expandable=True,
         Guidance=IF_ARGS [0] 30 0}]
Main.main5 =
  \ (w_a30U :: GHC.Base.String) ->
    Text.Read.$wread
      @ GHC.Types.Int
      (GHC.Read.$fReadInt2
       `cast` ((<Text.ParserCombinators.ReadPrec.Prec>
                -> Sym
                     (Text.ParserCombinators.ReadP.NTCo:ReadP <GHC.Types.Int>)) ; Sym
                                                                                    (Text.ParserCombinators.ReadPrec.NTCo:ReadPrec
                                                                                       <GHC.Types.Int>)
               :: (Text.ParserCombinators.ReadPrec.Prec
                   -> forall b_a3cL.
                      (GHC.Types.Int -> Text.ParserCombinators.ReadP.P b_a3cL)
                      -> Text.ParserCombinators.ReadP.P b_a3cL)
                    ~#
                  Text.ParserCombinators.ReadPrec.ReadPrec GHC.Types.Int))
      w_a30U

Main.main2
  :: GHC.Prim.State# GHC.Prim.RealWorld
     -> [GHC.Base.String]
     -> (# GHC.Prim.State# GHC.Prim.RealWorld, () #)
[GblId,
 Arity=2,
 Str=DmdType LL,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Arity=2, Value=True,
         ConLike=True, Cheap=True, Expandable=True,
         Guidance=IF_ARGS [0 30] 640 0}]
Main.main2 =
  \ (new_s_a1yC :: GHC.Prim.State# GHC.Prim.RealWorld)
    (a_a1yD :: [GHC.Base.String]) ->
    case GHC.IO.Handle.FD.openFile1
           (case a_a1yD of _ {
              [] -> GHC.List.badHead @ GHC.Base.String;
              : x_a13G ds1_a13H -> x_a13G
            })
           GHC.IO.IOMode.ReadMode
           new_s_a1yC
    of _ { (# new_s1_a2C3, a1_a2C4 #) ->
    case GHC.IO.Handle.Text.hGetContents1 a1_a2C4 new_s1_a2C3
    of _ { (# new_s2_X1Cz, a2_X1CB #) ->
    case Data.Time.Clock.POSIX.getPOSIXTime1 new_s2_X1Cz
    of _ { (# new_s3_a1yT, a3_a1yU #) ->
    case GHC.IO.Handle.Text.hPutStr2
           GHC.IO.Handle.FD.stdout
           (case GHC.Base.map
                   @ [GHC.Types.Char]
                   @ GHC.Types.Int
                   Main.main5
                   (Data.List.words a2_X1CB)
            of _ {
              [] ->
                Main.main4
                `cast` (UnsafeCo (GHC.Types.Int, [GHC.Types.Int]) [GHC.Types.Char]
                        :: (GHC.Types.Int, [GHC.Types.Int]) ~# [GHC.Types.Char]);
              : problemCount_aD7 numbers_aD8 ->
                case problemCount_aD7 of _ { GHC.Types.I# ww1_s38a ->
                Main.main_showProblem
                  Main.main3 (Main.$wbuildProblems ww1_s38a numbers_aD8)
                }
            })
           GHC.Types.True
           new_s3_a1yT
    of _ { (# new_s4_X1Fi, _ #) ->
    case Data.Time.Clock.POSIX.getPOSIXTime1 new_s4_X1Fi
    of _ { (# new_s5_X1CR, a5_X1CT #) ->
    GHC.IO.Handle.Text.hPutStr2
      GHC.IO.Handle.FD.stdout
      (case Data.Time.Clock.POSIX.$wposixSecondsToUTCTime a5_X1CT
       of _ { (# ww1_a31H, ww2_a31I #) ->
       case Data.Time.Clock.POSIX.$wposixSecondsToUTCTime a3_a1yU
       of _ { (# ww4_X34t, ww5_X34v #) ->
       GHC.Base.++
         @ GHC.Types.Char
         (Data.Fixed.showFixed
            @ Data.Fixed.E12
            (Data.Fixed.$fHasResolutionE12_$cresolution
             `cast` (Sym (Data.Fixed.NTCo:HasResolution <Data.Fixed.E12>)
                     :: (forall (p_a1O4 :: * -> *).
                         p_a1O4 Data.Fixed.E12 -> GHC.Integer.Type.Integer)
                          ~#
                        Data.Fixed.HasResolution Data.Fixed.E12))
            GHC.Types.True
            ((GHC.Integer.Type.minusInteger
                (Data.Time.Clock.POSIX.$wutcTimeToPOSIXSeconds ww1_a31H ww2_a31I)
                (Data.Time.Clock.POSIX.$wutcTimeToPOSIXSeconds ww4_X34t ww5_X34v))
             `cast` (Sym (Data.Fixed.NTCo:Fixed <Data.Fixed.E12>)
                     :: GHC.Integer.Type.Integer ~# Data.Fixed.Fixed Data.Fixed.E12)))
         Data.Time.Clock.UTC.$fShowNominalDiffTime2
       }
       })
      GHC.Types.True
      new_s5_X1CR
    }
    }
    }
    }
    }

Main.main1
  :: GHC.Prim.State# GHC.Prim.RealWorld
     -> (# GHC.Prim.State# GHC.Prim.RealWorld, () #)
[GblId,
 Arity=1,
 Str=DmdType L,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Arity=1, Value=True,
         ConLike=True, Cheap=True, Expandable=True,
         Guidance=IF_ARGS [0] 200 0}]
Main.main1 =
  \ (eta_B1 :: GHC.Prim.State# GHC.Prim.RealWorld) ->
    case System.Environment.getArgs2 eta_B1
    of _ { (# new_s_a1Oe, a2_a1Of #) ->
    case a2_a1Of of _ {
      Data.Maybe.Nothing ->
        case GHC.Environment.getFullArgs1 new_s_a1Oe
        of _ { (# new_s1_a1Ow, a3_a1Ox #) ->
        Main.main2
          new_s1_a1Ow
          (case System.Environment.dropRTSArgs a3_a1Ox of _ {
             [] -> GHC.List.tail1 @ GHC.Base.String;
             : _ xs_a1OD -> xs_a1OD
           })
        };
      Data.Maybe.Just argv_a1OG ->
        Main.main2
          new_s_a1Oe
          (case argv_a1OG of _ {
             [] -> GHC.List.tail1 @ GHC.Base.String;
             : ds1_a1OL xs_a1OM -> xs_a1OM
           })
    }
    }

Main.main :: GHC.Types.IO ()
[GblId,
 Arity=1,
 Str=DmdType L,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Arity=0, Value=True,
         ConLike=True, Cheap=True, Expandable=True,
         Guidance=ALWAYS_IF(unsat_ok=True,boring_ok=True)}]
Main.main =
  Main.main1
  `cast` (Sym (GHC.Types.NTCo:IO <()>)
          :: (GHC.Prim.State# GHC.Prim.RealWorld
              -> (# GHC.Prim.State# GHC.Prim.RealWorld, () #))
               ~#
             GHC.Types.IO ())

Main.main6
  :: GHC.Prim.State# GHC.Prim.RealWorld
     -> (# GHC.Prim.State# GHC.Prim.RealWorld, () #)
[GblId,
 Arity=1,
 Str=DmdType L,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Arity=1, Value=True,
         ConLike=True, Cheap=True, Expandable=True,
         Guidance=IF_ARGS [0] 30 0}]
Main.main6 =
  \ (eta_X1w :: GHC.Prim.State# GHC.Prim.RealWorld) ->
    GHC.TopHandler.runMainIO1
      @ ()
      (Main.main1
       `cast` (Sym (GHC.Types.NTCo:IO <()>)
               :: (GHC.Prim.State# GHC.Prim.RealWorld
                   -> (# GHC.Prim.State# GHC.Prim.RealWorld, () #))
                    ~#
                  GHC.Types.IO ()))
      eta_X1w

:Main.main :: GHC.Types.IO ()
[GblId,
 Arity=1,
 Str=DmdType L,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Arity=0, Value=True,
         ConLike=True, Cheap=True, Expandable=True,
         Guidance=ALWAYS_IF(unsat_ok=True,boring_ok=True)}]
:Main.main =
  Main.main6
  `cast` (Sym (GHC.Types.NTCo:IO <()>)
          :: (GHC.Prim.State# GHC.Prim.RealWorld
              -> (# GHC.Prim.State# GHC.Prim.RealWorld, () #))
               ~#
             GHC.Types.IO ())


------ Local rules for imported ids --------
"SC:$cshowsPrec0" [ALWAYS]
    forall (sc_s38W :: GHC.Prim.Int#) (sc1_s38X :: Main.Vine).
      Main.$fShowVine_$cshowsPrec (GHC.Types.I# sc_s38W) sc1_s38X
      = Main.$fShowProblem_$s$cshowsPrec sc_s38W sc1_s38X


Linking main.exe ...
