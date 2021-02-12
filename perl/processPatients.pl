#!/usr/bin/perl
use strict;

use CGI qw(:standard);
use CGI::Carp qw(warningsToBrowser fatalsToBrowser); 
print "Content-type: text/html\r\n\r\n";
my $cgi = new CGI;
my $gene=$cgi->param('Gene');

my $probes=$cgi->param('probes');

if ($probes eq "") {
	$probes = "no";
}

my $submit = $cgi->param('ss');


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
#my $b = 0;
my $b = busy();
if ($b) {
 
 #function will produce please wait message;
} else {

############################################################
##  Patients						  ##
############################################################

   my $gene=$cgi->param('GenePatients');
   $gene = uc($gene);
   CheckGene($gene);
   my $geneid = $gene . "-" . time();

   my $cut = $cgi->param('cut');
   my $endpoint = $cgi->param('endpoint');

   my $mmethod = $cgi->param('m');
   my $pmethod = $cgi->param('p');

   my $keepTreated = $cgi->param('KEEP.TREATED');


   #   my $cmd = "export JAVA_TOOL_OPTIONS=-Xss1280k; Rscript /usr/lib/cgi-bin/R/WEB.patients.R $gene $geneid $probes $cut $endpoint $mmethod $pmethod $keepTreated";
   my $cmd = "Rscript /usr/lib/cgi-bin/R/WEB.patients.R $gene $geneid $probes $cut $endpoint $mmethod $pmethod $keepTreated";
#print $cmd;
  system($cmd);
  
  $mmethod = uc $mmethod; 
  $cut = uc $cut; 
 
  if ($pmethod eq 'w') {
	$pmethod = "Wilcoxon Rank-Sum Test";
  } 
  if ($pmethod eq 't') {
     $pmethod = "t-test";
  }
 
  #######################################
  ## PIE CHART CODE
  #######################################

print << "EOF";

<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
<html>
<head>
	<meta http-equiv="Content-Type" content="text/html; charset=utf-8">
	<title>Results </title>
	<style type="text/css">

	#placeholdertumor, #placeholdergrade, #placeholderstage, #placeholderdss1,
		#placeholderdss2, #placeholderdss3 {
		width: 250px; 
		height: 130px;
	}

	td {text-align: center}
	table, td {border: 1px solid black}

	.upsig {background-color: red}
	.downsig {background-color: blue}
	.upnonsig {background-color: pink}
 	.downnonsig {background-color: #00FFFF}

	.td2 {border: 0px solid black}
	</style>

	<!--[if lte IE 8]><script language="javascript" type="text/javascript" src="/BCBET2/flot/scripts/excanvas.min.js"></script><![endif]-->
	<script language="javascript" type="text/javascript" src="/BCBET2/flot/scripts/jquery.js"></script>
	<script language="javascript" type="text/javascript" src="/BCBET2/flot/scripts/jquery.flot.js"></script>
	<script language="javascript" type="text/javascript" src="/BCBET2/flot/scripts/jquery.flot.pie.js"></script>
	<script type="text/javascript">

function labelFormatter(label, series) {
                return "<div style='font-size:8pt; text-align:center; padding:2px; color:white;'>" + label + "<br/>" + Math.round(series.percent) + "%</div>";
}

function pieplot(place, data) {
  \$(document).ready(function () {
	\$.plot(\$(place), data, {
		series: {
			pie: { 
				show: true,
				radius: 1,
				label: {
					show: true,
					radius: .5,
					formatter: function (label, series) {
                        return '<div style="font-size:8pt;text-align:center;padding:2px;color:black;">' + series.data[0][1] + '</div>';
                    },

					background: {
						opacity: .8,
						color: '#FFF'
					}
				}
			}
		},
		legend: {
			show: false
		}
	});
  });
};

</script>

<script language="javascript" type="text/javascript" src="/BCBET2/js/$geneid.js"></script>

</head>

<body>
        <center>
    <h2> PATIENT ANALYSIS FOR $gene </h2>

     <p>  <a href = "/BCBET2/xlsx/$geneid.xlsx"> Download Results</a> |  
     <a href="/BCBET2/">Back to BC-BET Homepage</a> </p>

	<table>
	<tr> 
		<td> Tumor vs. Normal <br> View Results 
		<input type = "radio" name = "tumor" value = "tumor" onClick = showTumor() checked = "checked" >  
		</td> 
		<td> High Grade vs. Low Grade <br> View Results 
		<input type = "radio" name = "tumor" value = "grade" onClick = showGrade() > 
		</td>
		<td> Muscle vs. non-muscle invasive <br> View Results 
		<input type = "radio" name = "tumor" value = "stage" onClick = showStage() > 
	        </td>
	</tr>
	<tr> </tr>
	<tr>
		<td> <div id="placeholdertumor"> </td>
		<td> <div id="placeholdergrade"> </td>
		<td> <div id="placeholderstage"> </td>
	</tr>
	</table>
	<br>
	<div id = "tumorTable"> </div>
	<br>  

	<table>
	<tr> 
		<td>Poor vs. Good Prognosis<br>(NMI+MI patients) <br> View Results
		<input type = "radio" name = "survival" value = "survival" onClick = showSurvivalAll() checked = "checked" >  
</td>
		<td> Poor vs. Good Prognosis<br>(LG, NMI patients) <br> View Results 
		<input type = "radio" name = "survival" value = "LGNMI" onClick = showSurvivalLGNMI() >  
</td>
		<td> Poor vs. Good Prognosis<br>(HG, MI patients) <br> View Results
		<input type = "radio" name = "survival" value = "HGMI" onClick = showSurvivalHGMI() >  
</td>
	</tr>
	<tr> </tr>
	<tr> 
		<td> <div id="placeholderdss1"> </td>
		<td> <div id="placeholderdss2"> </td>
		<td> <div id="placeholderdss3"> </td>
	</tr>
	</table>
	<br>
	<div id = "survivalTable"> </div>
	<br>  

<!-- END RESULTS -->

<table style = "font-size: smaller">
<tr>
<td style = "width: 32%">
     <table RULES = ROWS FRAME = BOX CELLPADDING = 3>
     <tr>
     <th class = "td2" colspan = "2"> BC-BET parameters </th>
     </tr>
     <tr> <td class = "td2"> Comparison Measure </td> 
          <td class = "td2">$mmethod</td></tr>  
     <tr> <td class = "td2"> P-value </td> 
          <td class = "td2">$pmethod</td> </tr>
     <tr> <td class = "td2"> Survival Endpoint </td> 
          <td class = "td2">$endpoint</td></tr>
     <tr> <td class = "td2"> Cutpoint </td> 
          <td class = "td2"> $cut </td> </tr> 
     <tr> <td class = "td2"> Include Treated Patients</td> 
          <td class = "td2"> $keepTreated </td> </tr> 
    </table>
</td>

<td style = "width:45%">
<center>
     Color codes correspond to whether gene is up- or down-regulated in tumors (compared to normal), high grade (compared to low grade) tumors, muscle invasive (compared to non-muscle invasive) tumors, and in patients with poor prognosis (compared to good prognosis), based on survival analysis. Pie chart numbers correspond to the number of datasets.
</td>
<td style = "width:23%"> 
     <center>
     <table>
     <tr> <td class = "upsig"> Up-regulated (P < 0.05) </td> </tr>
     <tr> <td class = "upnonsig"> Up-regulated (P > 0.05) </td> </tr>
     <tr><td class = "downnonsig"> Down-regulated (P > 0.05) </td> </tr>
     <tr> <td class = "downsig"> Down-regulated (P < 0.05) </td> </tr>
     </table>
     <center>
</center>
</td>
</tr>
</table>
     <p>  Click <a href = "/BCBET2/xlsx/$geneid.xlsx"> here</a> for a spreadsheet of the results. <br> 
     <p><a href="/BCBET2/">Look up another gene</a> </p>
      <p></p><p>

  </center>
  <script type="text/javascript">
  showTumor();
  showSurvivalAll();
  </script>

 </body>
 </html>
EOF
}
