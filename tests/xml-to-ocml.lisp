(in-package :ocml)

(defparameter *ex1*
 "<note>
  <date>2002-08-01</date>
  <to>Tove</to>
  <from>Jani</from>
  <heading>Reminder</heading>
  </note>") ;it works

(process-xml *ex1*)



(defparameter *ex2*
  "<person name=\"Jane\" age=\"29\"/>")  ; it works

(process-xml *ex2*)


(defparameter *listing*
  "<Listing>
          <StreetNumber>7</StreetNumber>
          <StreetName>WOODLEY HEADLAND</StreetName>
          <Town>PEARTREE BRIDGE MILTON KEYNES</Town>
          <PostCode>MK6 3PA</PostCode>
          <Country>UNITED KINGDOM</Country>
          <CountrySpecificLocalityLine> MILTON KEYNES</CountrySpecificLocalityLine>
          <DeliveryAddressLine> 7 WOODLEY HEADLAND</DeliveryAddressLine>
          <FormattedAddress>7 WOODLEY HEADLAND PEARTREE BRIDGE MILTON KEYNES MK6 3PA UNITED KINGDOM</FormattedAddress>
          <VerificationStatus>C</VerificationStatus>
          <VerificationStatusText>Data corrected</VerificationStatusText>
          <ResultPrecentage>99.75</ResultPrecentage>
          <ElementMatchStatus>43034000</ElementMatchStatus>
          <ElementMatchStatusTexts>Postal code: matched without errors, City: matched with errors, Province: empty, Street: matched with errors, Building number: matched without errors, PO Box: empty, Building: empty, Organization: empty</ElementMatchStatusTexts>
          <ElementResultStatus>63036000</ElementResultStatus>
          <ElementResultStatusTexts>Postal code: validated and unchanged, City: checked and corrected (changed or inserted), Province: empty, Street: checked and corrected (changed or inserted), Building number: validated and unchanged, PO Box: empty, Building: empty, Organization: empty </ElementResultStatusTexts>
  </Listing>")  ;it works

(process-xml *listing*)


(defparameter *cooking* 
  "<breakfast_menu>
	<food>
		<name>Belgian Waffles</name>
		<price>$5.95</price>
		<description>two of our famous Belgian Waffles with plenty of real maple syrup</description>
		<calories>650</calories>
	</food>
	<food>
		<name>Strawberry Belgian Waffles</name>
		<price>$7.95</price>

		<description>light Belgian waffles covered with strawberries and whipped cream</description>
		<calories>900</calories>
	</food>
	<food>
		<name>Berry-Berry Belgian Waffles</name>
		<price>$8.95</price>
		<description>light Belgian waffles covered with an assortment of fresh berries and whipped cream</description>
		<calories>900</calories>
	</food>
	<food>
		<name>French Toast</name>
		<price>$4.50</price>
		<description>thick slices made from our homemade sourdough bread</description>
		<calories>600</calories>
	</food>
	<food>
		<name>Homestyle Breakfast</name>
		<price>$6.95</price>
		<description>two eggs, bacon or sausage, toast, and our ever-popular hash browns</description>
		<calories>950</calories>
	</food>
</breakfast_menu>") 


(process-xml *cooking*)



(defparameter *ex10*
 "<a>
     <b>
       <c>
         <d d1=\"v1\"/>
         <d d1=\"v2\"/>
       </c>
       <c>
         <d d1=\"v3\"/>
       </c>
     </b>
  </a>")

(process-xml *ex10*)


(defparameter *ex3* 
  "<d a1=\"v1\" a2=\"v2\"/>")   ;; *ht*: (d (0 ((a1 v1) (a2 v2))))               ; IT WORKS!

(process-xml *ex3*)


(defparameter *ex4*
  "<c>
    <d a1=\"v11\" a2=\"v12\"/>
    <d a1=\"v21\" a2=\"v22\"/>
   </c>")                       ;; *ht*: (c (0 ((d 0) (d 1))))
                                ;;       (d (0 ((a1 v1) (a2 v2))) (1 ((a1 v1) (a2 v2))))
(process-xml *ex4*)


(defparameter *ex7*
  "<a>
     <b/>
     <b/>
   </a>") 


(process-xml *ex7*)


(defparameter *ex5*
  "<d a1=\"v1\"\"v2\"/>")    ;; *ht*: (d (1 ((a1 v1) ("value" v2))))          -> not addressed yet, even in the xsd2ocml!



(defparameter *ex6*
  "<a>
     <b>
       <c>
         <d d1=\"v1\"/>
       </c>
       <c>
         <d d1=\"v2\"/>
         <d d2=\"v3\"/>
       </c>
     </b>
     <b>
        <c>
          <d d1=\"v4\"/>
        </c>
        <c>
          <d d1=\"v5\"/>
          <d d2=\"v6\"/>
        </c>
      </b>
    </a>")                      ;; *ht*: (a (0 ((b 0) (b 1))))
                                ;;       (b (0 ((c 0) (c 1))) (1 ((c 2) (c 3))))
                                ;;       (c (0 ((d 0))) (1 ((d 1) (d 2))) (2 ((d 3))) (3 ((d 4) (d 5))))
                                ;;       (d (0 ((d0 v1))) (1 ((d1 v2))) (2 ((d2 v3))) (3 ((d1 v4))) (4 ((d1 v5))) (5 ((d2 v6)))) 
       
(process-xml *ex6*)
