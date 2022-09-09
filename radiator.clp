;;;======================================================
;;;   Car overheating and Coolant leaks Diagnosing system
;;;
;;;     This expert system diagnoses car overheating and 
;;;     Coolant leaking
;;;
;;;     CLIPS Version 6.3 Example
;;;
;;;     To execute, merely load, reset and run.
;;;======================================================

;;****************
;;* DEFFUNCTIONS *
;;****************

(deffunction ask-question (?question $?allowed-values)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer)

(deffunction yes-or-no-p (?question)
   (bind ?response (ask-question ?question yes no y n))
   (if (or (eq ?response yes) (eq ?response y))
       then yes 
       else no))

;;;***************
;;;* QUERY RULES *
;;;***************

(defrule determine-steam ""
   (not (steam-or-leak ?))
   (not (repair ?))
   =>
   (assert (steam-or-leak (yes-or-no-p "Is there steaming or leak (yes/no)? "))))
   
(defrule determine-smell ""
   (steam-or-leak no)
   (not (smell-antifreeze ?))
   (not (repair ?))
   =>
   (assert (smell-antifreeze (yes-or-no-p "Can you smell antifreeze without seeing steam or finding a leak (yes/no)? "))))

(defrule determine-needle-return ""
   (smell-antifreeze no)  
   (not (needle-return-normal ?)) 
   (not (repair ?))
   =>
   (assert (needle-return-normal (yes-or-no-p "Does temperature and needle return to normal  (yes/no)? "))))
   
(defrule determine-antifreeze-level ""
    (smell-antifreeze no)  
    (needle-return-normal no)
    (not (good-antifreeze-level ?)) 
    (not (repair ?))
   =>
   (assert ( good-antifreeze-level (yes-or-no-p "Is the antifreeze level correct (yes/no)? "))))
   
(defrule determine-cap-steaming ""
    (steam-or-leak yes)
    (not (cap-steam ?))
    (not (repair ?))
   =>
   (assert (cap-steam (yes-or-no-p "Is the radiator cap steaming and hissing (yes/no)? "))))

(defrule determine-overflow-dripping ""
   (steam-or-leak yes)
   (cap-steam no)
   (not (overflow-drip ?))
   (not (repair ?))
   =>
   (assert (overflow-drip (yes-or-no-p "Do you see drips on the ground from overflow reservoir (yes/no)? "))))

(defrule determine-radiator-leak ""
   (overflow-drip no)
   (not (radiator-leak ?))
   (not (repair ?))
   =>
   (assert (radiator-leak
               (yes-or-no-p "Is the radiator leaking from finned section (yes/no)? "))))

(defrule determine-hose-leak ""
   (overflow-drip no)
   (radiator-leak no)
   (not (hose-leak ?))
   (not (repair ?))
   =>
   (assert (hose-leak
              (yes-or-no-p "Is there a pinhole leak in hose or at clamps (yes/no)? "))))

(defrule determine-engine-leak ""
   (hose-leak no)
   (not (engine-leak ?))
   (not (repair ?))
   =>
   (assert (engine-leak
              (yes-or-no-p "Is antifreeze leaking from any engine surface (yes/no)? "))))

(defrule determine-water-pump-leak ""
   (engine-leak yes)
   (not (water-pump-leak ?))
   (not (repair ?))
   =>
   (assert (water-pump-leak
              (yes-or-no-p "Is antifreeze leaking from water pump (yes/no)? "))))    

(defrule determine-heater-core-leak ""
   (engine-leak no)
   (not (heater-core-leak ?))
   (not (repair ?))
   =>
   (assert (heater-core-leak
              (yes-or-no-p "Is there a heater core leak (yes/no)? "))))

(defrule determine-fan-operate ""
   (or (good-antifreeze-level yes)      
            (heater-core-leak no))
   (not (fan-operate ?))
   (not (repair ?))
   =>
    (assert (fan-operate
              (yes-or-no-p "Does the fan come on when the engine warms up (yes/no)? "))))

(defrule determine-coolant-flow ""
   (fan-operate yes)
   (not (coolant-flow ?))
   (not (repair ?))
   =>
   (assert (coolant-flow
              (yes-or-no-p "Is the coolant flowing through the radiator (yes/no)? "))))

(defrule determine-engine-flush""
   (coolant-flow yes)
   (not (engine-flush ?))
   (not (repair ?))
   =>
   (assert (engine-flush
              (yes-or-no-p "Have you tried super flushing the engine? (yes/no)? "))))

(defrule determine-thermostat""
   (engine-flush yes)
   (not (check-thermostat ?))
   (not (repair ?))
   =>
   (assert (check-thermostat
              (yes-or-no-p "Have you checked thermostat (yes/no)? "))))

(defrule determine-timing""
   (check-thermostat yes)
   (not (check-timing ?))
   (not (repair ?))
   =>
   (assert (check-timing
              (yes-or-no-p "Have you checked ignition timing, OBD codes (yes/no)? "))))

;;;****************
;;;* REPAIR RULES *
;;;****************

(defrule antifreeze-smell ""
   (smell-antifreeze yes)
   (not (repair ?))
   =>
   (assert (repair "It is a leak, you just haven't found it yet. Go to the previous step and choose as leak."))) 

(defrule return-normal ""
   (needle-return-normal yes)
   (not (repair ?))
   =>
   (assert (repair "Sticking, air-locked, or wrong temperature thermostant."))) 

(defrule wrong-antifreeze-level ""
   (good-antifreeze-level no)
   (not (repair ?))
   =>
   (assert (repair "Make-up with 50/50, but watch out!")))     

(defrule cap-steaming ""
   (cap-steam yes)
   (not (repair ?))
   =>
   (assert (repair "Pressure release working as intended. Check antifreeze level on overflow")))

(defrule overflow-drip ""
   (overflow-drip yes)
   (not (repair ?))
   =>
   (assert (repair "Best place for a drip, but indicates engine too hot or cooling system overfilled")))

(defrule radiator-leak ""
   (radiator-leak yes)
   (not (repair ?))
   =>
   (assert (repair "Problem even if overheated. Try stop-leak product or replace radiator")))

(defrule hose-leak ""
   (hose-leak yes)
   (not (repair ?))
   =>
   (assert (repair "Replace hose or shorten and reclamp if leak is under clamp")))

(defrule water-pump-leak""
   (engine-leak yes)
   (water-pump-leak yes)
   (not (repair ?))
   =>
   (assert (repair "Leak at water pump almost always means water pump failure.")))

(defrule gasket-problem ""
   (engine-leak yes)
   (water-pump-leak no)
   (not (repair ?))
   =>
   (assert (repair "Remove leaking part and installing with new gasket")))

(defrule heater-core-leak ""
   (heater-core-leak yes)
   (not (repair ?))
   =>
   (assert (repair "Heater core hoses, prssure test core, repair or replace")))

(defrule fan-problem ""
  (fan-operate no)
  (not (repair ?))
  =>
  (assert (repair "Test fan motor with direct connection, check for fan fuse, replace temperature sensor")))

  (defrule coolant-flow ""
  (coolant-flow no)
  (not (repair ?))
  =>
  (assert (repair "Pump failure or blockage")))

(defrule Flushed-engine ""
  (engine-flush no)
  (not (repair ?))
  =>
  (assert (repair "Flush engine with kit, cleaning solution and garden hose, fill with new 50/50 antifreeze")))

(defrule checked-thermostat ""
  (check-thermostat no)
  (not (repair ?))
  =>
  (assert (repair "Throw the thermostat in boiling water and see if it opens. Or just replace it.")))

  (defrule unchecked-timing ""
  (check-timing  no)
  (not (repair ?))
  =>
  (assert (repair "Improper timming can lead to overheating")))

  (defrule checked-timing ""
  (check-timing  yes)
  (not (repair ?))
  =>
  (assert (repair "If occasional overheating, may be overdriving. Otherwise, suspect improper thermostant installation.")))

;;;********************************
;;;* STARTUP AND CONCLUSION RULES *
;;;********************************

(defrule system-banner ""
  (declare (salience 10))
  =>
  (printout t crlf crlf)
  (printout t "The Engine Overheating Diagnosis Expert System")
  (printout t crlf crlf))

(defrule print-repair ""
  (declare (salience 10))
  (repair ?item)
  =>
  (printout t crlf crlf)
  (printout t "Suggested Repair:")
  (printout t crlf crlf)
  (format t " %s%n%n%n" ?item))
