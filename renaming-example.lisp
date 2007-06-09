(def-class super1 ()((slot1)))

(def-class superA ()((slota)))

(def-class super2 (super1 supera)
  ((slot2)
   (slotb))
  :slot-renaming ((slotb slota)(slot2 slot1)))

(def-class super3 (super1)((slot3 :value 3))
  :slot-renaming ((slot3 slot1)))

(def-class super4 (super3 super2)((slot4))
  :slot-renaming ((slot4 slot2)))

(def-class super-aux ()((slot-aux)))

(def-class super5 (super4 super-aux)
  ((slot5))
  :slot-renaming ((slot5 slot4)(slot5 slot-aux)))


;;;;;;;;;;;;
(def-class superaa ()((slot-a)))


(def-class superbb (superaa)((slot-b)):slot-renaming ((SLOT-b slot-a)))

(def-class supercc (superaa)((slot-c)):slot-renaming ((slot-c slot-a)))

(def-class superdd (superbb supercc)((slot-p)(slot-q))
  :slot-renaming ((slot-q slot-c)(slot-p slot-b)))







