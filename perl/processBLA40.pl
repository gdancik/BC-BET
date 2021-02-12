#!/usr/bin/perl
###if (0) {
use CGI qw(:standard);
use CGI::Carp qw(warningsToBrowser fatalsToBrowser); 
use strict;

print "Content-type: text/html\r\n\r\n";
my $cgi = new CGI;
my $gene=$cgi->param('Gene');


$gene = uc($gene);

sub CheckGene {
  if ($_[0] eq "") {
     print("Error: You must enter a gene name\n");
     print "<br><br> <a href=\"/BCBET2/\">Back</a>";
     exit(-1);
  }
}


#############################################################
# make sure server is not busy
#############################################################

sub busy () {
  my $numjobs = `ps -ef | grep /usr/lib/R/bin/exec/R | wc -l`;
  if ($numjobs > 5) {
   
    print "<html><body>"; 
    print "<p style = \"background-color: lightgreen; font-size:120\%; font-weight: bold; font-color:white\">";
    print "<br>Sorry, the server is currently experiencing a very high load. Please try again later. </p>";
    print "<p> <a href = \"../BCBET2\"> BACK </a>";
    print "</body></html>";

    return 1;
  } 
  return 0;
}

my $b = busy();
if ($b) {
 #function will produce please wait message;
} else {

   my $gene=$cgi->param('GeneBLA40');
   $gene = uc($gene);
   CheckGene($gene);

   my $gene2 = $cgi->param('Gene2BLA40');
   $gene2 = uc($gene2);
	
   if ($gene2 eq "") {
     CheckGene($gene);

    my $cmd = "/usr/bin/Rscript /usr/lib/cgi-bin/R/WEB.bla40.R $gene";
    system($cmd);

    my $img_tumor_pdf = "/img/$gene.pdf";
    my $img_tumor_png = "/img/$gene.png";

    my $f = "/var/www/html/BCBET2$img_tumor_pdf";

    if (-e $f) {
      my $cmd = "convert /var/www/html/BCBET2$img_tumor_pdf /var/www/html/BCBET2$img_tumor_png";
      system($cmd);
      my $cmd = "rm /var/www/html/BCBET2$img_tumor_pdf";
      system($cmd);
      print "<IMG SRC = \"/BCBET2$img_tumor_png\">";   
    }  
    print "<p><a href=\"/../BCBET2/BLA40/\">Perform another BLA-40 analysis</a> </p>";
   print "<p><a href=\"/../BCBET2/\">BC-BET homepage</a> </p>";
  } else {
    my $cmd = "/usr/bin/Rscript /usr/lib/cgi-bin/R/WEB.bla40.cor.R $gene $gene2 $gene-$gene2";
    system($cmd);


  ########################################
  ## SCATTERPLOT (CORRELATION PLOT) CODE 
  ########################################

   print "
   



<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">
<html>
<head>

<style type = \"text/css\">
td {padding: 3px}
</style>

	<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">
	<title>BLA-40 Analysis </title>
	<!--[if lte IE 8]><script language=\"javascript\" type=\"text/javascript\" src=\"/BCBET2/flot/scripts/excanvas.min.js\"></script><![endif]-->
	<script language=\"javascript\" type=\"text/javascript\" src=\"/BCBET2/flot/scripts/jquery.js\"></script>
	<script language=\"javascript\" type=\"text/javascript\" src=\"/BCBET2/flot/scripts/jquery.flot.js\"></script>
	<script language=\"javascript\" type=\"text/javascript\" src=\"/BCBET2/flot/scripts/jquery.flot.axislabels.js\"></script>
	<script language=\"javascript\" type=\"text/javascript\" src=\"/BCBET2/js/$gene-$gene2.js\"></script>
	<script type=\"text/javascript\">

	\$(function() {

		var i = 0;
		\$.each(datasets, function(key, val) {
			val.color = \"red\";
			++i;
		});


		// get selection and display graph
		var choiceContainer = \$(\"#choices\");
		choiceContainer.find(\"input\").click(plotAccordingToChoices);

		function plotAccordingToChoices() {

			var data = [];

			choiceContainer.find(\"input:checked\").each(function () {
				var key = \$(this).attr(\"value\");
				if (key && datasets[key]) {
					data.push(datasets[key]);
				}
				if (key == \"p\") {
				  \$( \"#title\" ).html(\"<center> Gene Expression </center>\" ) 				
				} 
				if (key == \"s\") {
				  \$( \"#title\" ).html(\"<center> Rank Expression </center>\" ) 				
				} 
			});

			if (data.length > 0) {
				\$.plot(\"#placeholder\", data, {
					series: {
						points: {
							show: true,
							radius: 3
						}
					},
					yaxis: {
						axisLabel: ylab,
						axisLabelUseCanvas: true,
						axisLabelPadding:5,
						axisLabelFontSizePixels: 12 
					},
					xaxis: {
						tickDecimals: 0,
						axisLabel: xlab,
						axisLabelPadding: 5,
						axisLabelFontSizePixels: 12
					}
				});
			}
		}

		plotAccordingToChoices();


	});

	</script>
</head>
<body>
<center>
<h2> BLA-40 Analysis: Co-expression of $gene and $gene2 </h2>
<table> <tr>

	<td> 
	<h3 id = \"title\">  </h3> 
	<div id=\"placeholder\" style=\"float:left; width:500px; height:400px\"></div> </td>

	<td>
	<table id = \"choices\" border = \"1\" style = \"border-collapse: collapse\">
	<tr>
	<td> Display </td> <td> Correlation (p-value) </td>
	</tr>
	<tr>
	<td><input type=\"radio\" name=\"type\" value=\"p\" checked = \"checked\">Pearson Correlation </td>
	<td id = \"resPearson\"> </td> 
	</tr>
	<tr>
	<td><input type=\"radio\" name=\"type\" value=\"s\">Spearman (rank) correlation </td>
	<td id = \"resSpearman\"> </td> 
	</tr>
	</table>
	</td>
</tr> </table>
<p><a href=\"/../BCBET2/BLA40/\">Perform another BLA-40 analysis</a> </p>
<p><a href=\"/../BCBET2/\">BC-BET homepage</a> </p>
</center>
</body>
</html>
 ";

  }
}
