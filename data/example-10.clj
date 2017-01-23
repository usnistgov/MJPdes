(map->Model
 {:line 
  {:m1 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W {:dist :uniform :bounds [0.8, 1.2]}}) 
   :b1 (map->Buffer {:N 3})
   :m2 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W {:dist :uniform :bounds [0.8, 1.2]}})
   :b2 (map->Buffer {:N 5})
   :m3 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W {:dist :uniform :bounds [0.8, 1.2]}})
   :b3 (map->Buffer {:N 1})
   :m4 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W {:dist :uniform :bounds [0.8, 1.2]}})
   :b4 (map->Buffer {:N 1})
   :m5 (map->ExpoMachine {:lambda 0.1 :mu 0.9 :W {:dist :uniform :bounds [0.8, 1.2]}})}
  :number-of-simulations 10
  :topology [:m1 :b1 :m2 :b2 :m3 :b3 :m4 :b4 :m5]
  :entry-point :m1 ; 
  :params {:warm-up-time 2000 :run-to-time 20000}  
  :jobmix {:jobType1 (map->JobType {:portion 1.0
                                    :w {:m1 1.0,
                                        :m2 1.0,
                                        :m3 1.0,
                                        :m4 1.0,
                                        :m5 1.01}})}})
