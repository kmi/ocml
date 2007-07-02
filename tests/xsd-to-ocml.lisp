(in-package :ocml)


(defparameter *producer* 
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<schema xmlns=\"http://www.w3.org/2001/XMLSchema\" xmlns:tns=\"http://localhost:8080/ProducerXMLSchema\" targetNamespace=\"http://localhost:8080/ProducerXMLSchema\">

	<complexType name=\"ProductConfirmation\">
		<sequence>
			<element name=\"theConfirmation\" type=\"int\"/>
			<element name=\"thePrice\" type=\"float\"/>
		</sequence>
	</complexType>

	<complexType name=\"ProductId\">
		<sequence>
			<element name=\"brand\" type=\"string\"/>
			<element name=\"reference\" type=\"string\"/>
		</sequence>
	</complexType>

	<complexType name=\"ProductDetails\">
		<sequence>
			<element name=\"price\" type=\"float\"/>
			<element name=\"quoteId\" type=\"string\"/>
			<element name=\"size\" type=\"int\"/>
		</sequence>
	</complexType>

</schema>")

(process-xsd *producer*)



(defparameter *ex5*                                                  
"<schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\"> 
<element name=\"E1\" type=\"C1\"/>
<complexType name=\"C1\">
  <complexContent>
   <extension base=\"C2\">	
    <sequence>	
     <element name=\"E11\" type=\"T11\" minOccurs=\"1\" />
     <element name=\"E12\" type=\"T12\"/>
    </sequence>
   </extension>
  </complexContent>
</complexType>

<complexType name=\"C2\">
  <complexContent>
   <extension base=\"C3\">	
    <sequence>	
     <element name=\"E21\" type=\"T21\"/>
     <element name=\"E22\" type=\"T22\"/>
    </sequence>
   </extension>
  </complexContent>
</complexType>

<complexType name=\"C3\">
  <complexContent>
   <extension base=\"C4\">	
    <sequence>	
     <element name=\"E31\" type=\"T31\"/>
     <element name=\"E32\" type=\"T32\"/>
    </sequence>
   </extension>
  </complexContent>
</complexType>
</schema>")

(process-xsd *ex5*)           

(defparameter *ex* 
"<element name=\"E\" type=\"xs:string\"/>")

(process-empty-node (xmlp::root (xmlp::document-parser *ex*)))



(defparameter *xsd-string* 
"<schema>
 <element name=\"shipto\">
  <complexType>
   <sequence>
     <element name=\"name\" type=\"NameType\"/>
     <element name=\"address\" type=\"AddressType\"/>
     <element name=\"city\" type=\"CityType\"/>
     <element name=\"country\" type=\"CountryType\"/>
   </sequence>
  </complexType>
 </element>
 <element name=\"item\">
  <complexType>
   <sequence>
     <element name=\"title\" type=\"TitleType\"/>
     <element name=\"note\" type=\"NoteType\"/>
     <element name=\"quantity\" type=\"QuantityType\"/>
     <element name=\"price\" type=\"PriceType\"/>
   </sequence>
  </complexType>
 </element>
 <element name=\"smthg\" type=\"string\"/>
 <element name=\"person\">
   <complexType>
    <sequence>
      <element name=\"Name\" type=\"string\"/>
      <element name=\"Age\" type=\"positiveInteger\"/>
    </sequence>
   </complexType>
  </element>
</schema>")  


(process-xsd *xsd-string* 'RoxOnt)                       

(defparameter *ex3* 
"<schema>

<element name=\"shiporder\">
 <complexType>
   <attribute ref=\"orderid\" use=\"required\"/>
   <sequence>
    <element ref=\"orderperson\"/>
    <element ref=\"name\"/>
   </sequence>
 </complexType>
</element>

<attribute name=\"orderid\" type=\"string\"/>
<element name=\"orderperson\" type=\"string\"/>
<element name=\"name\" type=\"string\"/>
</schema>") 


(process-xsd *ex3* 'RoxOnt)       

(defparameter *ex* 
"<schema>
<element name=\"airmail\" type=\"airmailorder\"/>
<complexType name=\"airmailorder\">
  <complexContent>
   <extension base=\"shiporder\">	
    <sequence>	
     <element name=\"cargo\" type=\"string\"/>
     <element name=\"extra-fare\" type=\"decimal\"/>
    </sequence>
   </extension>
  </complexContent>
</complexType>

<complexType name=\"airmailorder2\">
  <complexContent>
   <extension base=\"airmailorder\">	
    <sequence>	
     <element name=\"cargo2\" type=\"string\"/>
     <element name=\"extra-fare2\" type=\"decimal\"/>
    </sequence>
   </extension>
  </complexContent>
</complexType>
</schema>")

(process-xsd *ex* 'RoxOnt)                                  


(defparameter *ex2*
"<schema>

<element name=\"orderperson\" type=\"string\"/>
<element name=\"name\" type=\"string\"/>
<element name=\"address\" type=\"string\"/>
<element name=\"city\" type=\"string\"/>
<element name=\"country\" type=\"string\"/>
<element name=\"title\" type=\"string\"/>
<element name=\"note\" type=\"string\"/>
<element name=\"quantity\" type=\"positiveInteger\"/>
<element name=\"price\" type=\"decimal\"/>
<attribute name=\"orderid\" type=\"string\"/>

<element name=\"shipto\">
 <complexType>
  <sequence>
   <element ref=\"name\"/>
   <element ref=\"address\"/>
   <element ref=\"city\"/>
   <element ref=\"country\"/>
  </sequence>
 </complexType>
</element>
<element name=\"item\">
 <complexType>
  <sequence>
   <element ref=\"title\"/>
   <element ref=\"note\" minOccurs=\"0\"/>
   <element ref=\"quantity\"/>
   <element ref=\"price\"/>
  </sequence>
 </complexType>
</element>
<element name=\"shiporder\">
 <complexType>
  <sequence>
   <element ref=\"orderperson\"/>
   <element ref=\"shipto\"/>
   <element ref=\"item\" maxOccurs=\"unbounded\"/>
  </sequence>
  <attribute ref=\"orderid\" use=\"required\"/>
 </complexType>
</element>

<complexType name=\"item2\">
 <sequence>
   <element ref=\"title\"/>
   <element ref=\"note\" minOccurs=\"0\"/>
   <element ref=\"quantity\"/>
   <element ref=\"price\"/>
  </sequence>
 </complexType>

</schema>")
(process-xsd *ex2* 'RoxOnt)        


(defparameter *x* "<schema> <attribute name=\"Name\" use=\"required\"/></schema>")

(process-xsd *x* 'RoxOnt)        ;; this returns nil. Is this correct? Should we create ocml code for 'top-class as well?
