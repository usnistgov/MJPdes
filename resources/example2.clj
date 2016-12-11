
(map->Model
 {:line 
  {:m1 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.2 }) 
   :b1 (map->Buffer {:N 3})
   :m2 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.0 })
   :b2 (map->Buffer {:N 5})
   :m3 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.1 })
   :b3 (map->Buffer {:N 1})
   :m4 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.05 })
   :b4 (map->Buffer {:N 1})
   :m5 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W 1.2 })}
  :topology [:m1 :b1 :m2 :b2 :m3 :b3 :m4 :b4 :m5]
  :entry-point :m1 
  :params {:warm-up-time 2000 :run-to-time 20000}  
  :jobmix {:jobType1 (map->JobType {:portion 1.0
                                    :w {:m1 1.0, :m2 1.0, :m3 1.0, :m4 1.0, :m5 1.0}})}})


#_{:TP 0.83727777,
 :observed-cycle-time 12.96041327354959,
 :compared-starve
 {:m1 :na,
  :m2 0.9079997499270831,
  :m3 1.8512703418112915,
  :m4 1.6493364994293767,
  :m5 1.1552869035601778},
 :compared-block
 {:m1 0.5682899874479178,
  :m2 0.6623236916681731,
  :m3 0.5094236802539698,
  :m4 0.4697206089180695,
  :m5 :na},
 :wip
 {:b1 2.6600418926539375,
  :b2 2.77025590328012,
  :b3 0.613105159819217,
  :b4 0.17255712315467386},
 :number-of-jobs 15071,
 :computed-cycle-time 11.292252430802435,
 :status nil,
 :runtime 18000,
 :bneck :m2,
 :starved
 {:m1 0.0,
  :m2 0.00808119777435104,
  :m3 0.03443362835769002, 
  :m4 0.0761993462736372,
  :m5 0.21488336406219308},
 :blocked
 {:m1 0.10570193766531272,
  :m2 0.022916399731718787,
  :m3 0.053285716954565236,
  :m4 0.018694880234939166,
  :m5 0.0}}
