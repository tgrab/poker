function FactoryXMLHttpRequest() {

if (window.XMLHttpRequest)
	return new XMLHttpRequest();
else if (window.ActiveXObject) {
	var msxmls=new Array(
	'Msxml2.XMLHTTP.5.0',
	'Msxml2.XMLHTTP.4.0',
	'Msxml2.XMLHTTP.3.0',
	'Msxml2.XMLHTTP',
	'Microsoft.XMLHTTP');
	for (var i=0;i<msxmls.length;i++) {
		try {
		return new ActiveXobject(msxmls[i]);
		}
		catch (e) {
		}
	}
}
alert('No Ajax');


}

var xmlhttp= FactoryXMLHttpRequest();

function setResult1(url) {
 if (xmlhttp) {
 xmlhttp.open('GET',url,false);
 xmlhttp.send(null);
 document.getElementById('result1').innerHTML=xmlhttp.responseText;
 }
}
