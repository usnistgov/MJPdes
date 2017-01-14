
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
  :jobmix {:jobType1 (map->JobType {:portion 0.8
                                    :w {:m1 1.0, :m2 1.0, :m3 1.0, :m4 1.0, :m5 1.0}})
           :jobType2 (map->JobType {:portion 0.2
                                    :w {:m1 1.0, :m2 2.0, :m3 1.5, :m4 1.0, :m5 1.0}})}})


