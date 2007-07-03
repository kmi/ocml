(in-package :ocml.tests)

(def-suite xml-to-ocml-suite)

(defun repackage (tree package)
  (labels ((repackage-thing (thing package)
	     (if (and (symbolp thing) (not (keywordp thing)))
		 (intern (symbol-name thing) package)
		 thing))
	   (tree-map (fn tree)
	     (if (consp tree)
		 (cons (tree-map fn (car tree))
		       (tree-map fn (cdr tree)))
		 (funcall fn tree))))
   (tree-map #'(lambda (x)
		 (repackage-thing x package))
	     tree)))

(defun ocml-equal? (ocml xml)
  (equal (repackage ocml :ocml)
	 (let ((*package* (find-package :ocml)))
	   (ocml:translate :xml :ocml xml))))

(test xml-to-ocml-test
  (is (ocml-equal? '((DEF-INSTANCE NOTE0 NOTE ((DATE :VALUE "2002-08-01") (TO :VALUE "Tove") (FROM :VALUE "Jani") (HEADING :VALUE "Reminder"))))
		   "<note><date>2002-08-01</date><to>Tove</to><from>Jani</from><heading>Reminder</heading></note>"))

  (is (ocml-equal? '((DEF-INSTANCE OCML::PERSON0 OCML::PERSON ((OCML::NAME :VALUE "Jane") (OCML::AGE :VALUE "29"))))
		   "<person name=\"Jane\" age=\"29\"/>"))

  (is (ocml-equal? '((DEF-INSTANCE OCML::LISTING0 OCML::LISTING
		      ((OCML::STREETNUMBER :VALUE "7")
		       (OCML::STREETNAME :VALUE "WOODLEY HEADLAND")
		       (OCML::TOWN :VALUE "PEARTREE BRIDGE MILTON KEYNES")
		       (OCML::POSTCODE :VALUE "MK6 3PA")
		       (OCML::COUNTRY :VALUE "UNITED KINGDOM")
		       (OCML::COUNTRYSPECIFICLOCALITYLINE :VALUE " MILTON KEYNES")
		       (OCML::DELIVERYADDRESSLINE :VALUE " 7 WOODLEY HEADLAND")
		       (OCML::FORMATTEDADDRESS :VALUE "7 WOODLEY HEADLAND PEARTREE BRIDGE MILTON KEYNES MK6 3PA UNITED KINGDOM")
		       (OCML::VERIFICATIONSTATUS :VALUE "C")
		       (OCML::VERIFICATIONSTATUSTEXT :VALUE "Data corrected")
		       (OCML::RESULTPRECENTAGE :VALUE "99.75")
		       (OCML::ELEMENTMATCHSTATUS :VALUE "43034000")
		       (OCML::ELEMENTMATCHSTATUSTEXTS :VALUE "Postal code: matched without errors, City: matched with errors, Province: empty, Street: matched with errors, Building number: matched without errors, PO Box: empty, Building: empty, Organization: empty")
		       (OCML::ELEMENTRESULTSTATUS :VALUE "63036000")
		       (OCML::ELEMENTRESULTSTATUSTEXTS :VALUE "Postal code: validated and unchanged, City: checked and corrected (changed or inserted), Province: empty, Street: checked and corrected (changed or inserted), Building number: validated and unchanged, PO Box: empty, Building: empty, Organization: empty ")
		       )))
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
  </Listing>"))
  (is (ocml-equal? '((OCML::DEF-INSTANCE OCML::FOOD0 OCML::FOOD ((OCML::NAME :VALUE "Belgian Waffles") (OCML::PRICE :VALUE "$5.95") (OCML::DESCRIPTION :VALUE "two of our famous Belgian Waffles with plenty of real maple syrup") (OCML::CALORIES :VALUE "650")))
		     (OCML::DEF-INSTANCE OCML::FOOD1 OCML::FOOD ((OCML::NAME :VALUE "Strawberry Belgian Waffles") (OCML::PRICE :VALUE "$7.95") (OCML::DESCRIPTION :VALUE "light Belgian waffles covered with strawberries and whipped cream") (OCML::CALORIES :VALUE "900")))
		     (OCML::DEF-INSTANCE OCML::FOOD2 OCML::FOOD ((OCML::NAME :VALUE "Berry-Berry Belgian Waffles") (OCML::PRICE :VALUE "$8.95") (OCML::DESCRIPTION :VALUE "light Belgian waffles covered with an assortment of fresh berries and whipped cream") (OCML::CALORIES :VALUE "900")))
		     (OCML::DEF-INSTANCE OCML::FOOD3 OCML::FOOD ((OCML::NAME :VALUE "French Toast") (OCML::PRICE :VALUE "$4.50") (OCML::DESCRIPTION :VALUE "thick slices made from our homemade sourdough bread") (OCML::CALORIES :VALUE "600")))
		     (OCML::DEF-INSTANCE OCML::FOOD4 OCML::FOOD ((OCML::NAME :VALUE "Homestyle Breakfast") (OCML::PRICE :VALUE "$6.95") (OCML::DESCRIPTION :VALUE "two eggs, bacon or sausage, toast, and our ever-popular hash browns") (OCML::CALORIES :VALUE "950")))
		     (OCML::DEF-INSTANCE OCML::BREAKFAST_MENU0 OCML::BREAKFAST_MENU ((OCML::FOOD :VALUE FOOD4) (OCML::FOOD :VALUE FOOD3) (OCML::FOOD :VALUE FOOD2) (OCML::FOOD :VALUE FOOD1) (OCML::FOOD :VALUE FOOD0))))
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
</breakfast_menu>"))

  (is (ocml-equal? '((ocml::def-instance ocml::a0 ocml::a ((ocml::b :value b0))) (ocml::def-instance ocml::b0 ocml::b ((ocml::c :value c1) (ocml::c :value c0))) (ocml::def-instance ocml::c0 ocml::c ((ocml::d :value d0) (ocml::d :value d1))) (ocml::def-instance ocml::c1 ocml::c ((ocml::d :value d2))) (ocml::def-instance ocml::d0 ocml::d ((ocml::d1 :value "v1"))) (ocml::def-instance ocml::d1 ocml::d ((ocml::d1 :value "v2"))) (ocml::def-instance ocml::d2 ocml::d ((ocml::d1 :value "v3"))))
		   "<a> <b> <c> <d d1=\"v1\"/> <d d1=\"v2\"/> </c> <c> <d d1=\"v3\"/> </c> </b> </a>"))

  (is (ocml-equal? '((ocml::def-instance ocml::c0 ocml::c ((ocml::d :value d0) (ocml::d :value d1)))
		     (ocml::def-instance ocml::d0 ocml::d ((ocml::a1 :value "v11") (ocml::a2 :value "v12")))
		     (ocml::def-instance ocml::d1 ocml::d ((ocml::a1 :value "v21") (ocml::a2 :value "v22"))))
		   "<c> <d a1=\"v11\" a2=\"v12\"/> <d a1=\"v21\" a2=\"v22\"/> </c>"))

  (is (ocml-equal? '((ocml::def-instance ocml::a0 ocml::a ((ocml::b :value b0) (ocml::b :value b1)))
		     (ocml::def-instance ocml::b0 ocml::b nil) (ocml::def-instance ocml::b1 ocml::b nil))
		   "<a> <b/> <b/> </a>"))

  (is (ocml-equal? '((OCML::DEF-INSTANCE OCML::A0 OCML::A ((OCML::B :VALUE B1) (OCML::B :VALUE B0)))
		     (OCML::DEF-INSTANCE OCML::B0 OCML::B ((OCML::C :VALUE C1) (OCML::C :VALUE C0)))
		     (OCML::DEF-INSTANCE OCML::B1 OCML::B ((OCML::C :VALUE C3) (OCML::C :VALUE C2)))
		     (OCML::DEF-INSTANCE OCML::C0 OCML::C ((OCML::D :VALUE D0)))
		     (OCML::DEF-INSTANCE OCML::C1 OCML::C ((OCML::D :VALUE D1) (OCML::D :VALUE D2)))
		     (OCML::DEF-INSTANCE OCML::C2 OCML::C ((OCML::D :VALUE D3)))
		     (OCML::DEF-INSTANCE OCML::C3 OCML::C ((OCML::D :VALUE D4)))
		     (OCML::DEF-INSTANCE OCML::C4 OCML::C ((OCML::D :VALUE D5)))
		     (OCML::DEF-INSTANCE OCML::D0 OCML::D ((OCML::D1 :VALUE "v1")))
		     (OCML::DEF-INSTANCE OCML::D1 OCML::D ((OCML::D1 :VALUE "v2")))
		     (OCML::DEF-INSTANCE OCML::D2 OCML::D ((OCML::D2 :VALUE "v3")))
		     (OCML::DEF-INSTANCE OCML::D3 OCML::D ((OCML::D1 :VALUE "v4")))
		     (OCML::DEF-INSTANCE OCML::D4 OCML::D ((OCML::D1 :VALUE "v5")))
		     (OCML::DEF-INSTANCE OCML::D5 OCML::D ((OCML::D2 :VALUE "v6"))))
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
    </a>")))
