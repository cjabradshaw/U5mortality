# Under-five mortality rate projections comparing <a href="https://korbel.du.edu/pardee/international-futures-platform/">International Futures</a>, <a href="https://www.un.org/development/desa/pd/">United Nations Population Division</a>, and <a href="https://iiasa.ac.at/">International Institute for Applied Systems Analysis</a> (IIASA) <a href="https://dataexplorer.wittgensteincentre.org/wcde-v3/">Wittgenstein Centre Population and Human Capital Projections 2023</a> (WIC2023)
<img align="right" src="www/pop.png" alt="population icon" width="180" style="margin-top: 20px">
<br>
Dr <a href="https://futurechildhealth.org/sanchita-gera/">Sanchita Gera</a><br>
<a href="https://hsph.harvard.edu">Harvard T.H. Chan School of Public Health</a> & <a href="https://futurechildhealth.org">Future Child Health</a><br>
<a href=mailto:Sanchita.Gera@thekids.org.au>e-mail</a> <br>
<br>
Prof <a href="https://globalecologyflinders.com/people/#DIRECTOR">Corey J. A. Bradshaw</a> <br>
<a href="http://globalecologyflinders.com" target="_blank">Global Ecology</a> | <em><a href="https://globalecologyflinders.com/partuyarta-ngadluku-wardli-kuu/" target="_blank">Partuyarta Ngadluku Wardli Kuu</a></em>, <a href="http://flinders.edu.au" target="_blank">Flinders University</a>, Adelaide, Australia <br>
<a href=mailto:corey.bradshaw@flinders.edu.au>e-mail</a> <br>
June 2026 <br>
<br>

Accompanies paper:<br>
<br>
<a href="https://futurechildhealth.org/sanchita-gera/">Gera, S</a>, <a href="https://futurechildhealth.org/team/dr-melinda-a-judge/">MA Judge</a>, <a href="https://scholar.google.com/citations?user=pr1F4mgAAAAJ&hl=en">S Sellers</a>, <a href="https://scholar.google.com/citations?hl=en&user=3gkoM_YAAAAJ">P Lucas</a>, <a href="https://globalecologyflinders.com/people/#DIRECTOR">CJA Bradshaw</a>, <a href="https://futurechildhealth.org/team/professor-peter-le-souef/">PN Le Souëf</a>. <a href="https://doi.org/">Child mortality projections under Shared Socioeconomic Pathways vastly underestimate future deaths</a>

## Abstract
Under-five child mortality is a consequential marker of population health and development. Over the last decade, the rate of decline in under-five mortality has slowed. Accelerating climate change poses a threat to child health and undermines current child mortality targets. Despite these undeniable realities, United Nations’ child mortality projections do not account for climate change. Our study extends current databases to generate new under-five mortality projections under Shared Socio-economic Pathway (SSP) scenarios to 2100. We found that globally, under-five mortality projections will not meet the Sustainable Development Goal target and will surpass current United National Population Division Projections under SSP3 and SSP4. International Futures projections produce more than 330 million excess under-five deaths compared to the United National Population Division Projections from 2030–2100. Under-five mortality under International Futures and the Wittgenstein Centre Population and Human Capital Projections 2023 (WIC2023) are as high as 2.69 and 2.88 times the rate in United National Population Division Projections, respectively. Due to the paucity of modelling studies, highly variable predictions across studies, optimistic assumptions of SSPs, and dangers of inaction from underestimating child mortality, there is a need to increase accuracy of current projections to inform climate-change policies and interventions. 

## <a href="https://github.com/cjabradshaw/U5mortality/tree/main/scripts">Scripts</a>
- <code>U5MR Projections.R</code> (main code)

## <a href="https://github.com/cjabradshaw/U5mortality/tree/main/data">Data</a>
- <em>births.csv</em>: <a href="https://data.un.org/Data.aspx?d=POP&f=tableCode%3a1">United Nations Population Division age structure data from 1950-2021</a>
- <em>deaths.csv</em>: <a href="https://data.un.org/Data.aspx?d=POP&f=tableCode%3a1">United Nations Population Division data for Angola</a>
- <em>IF_births.csv</em>: <a href="https://data.un.org/Data.aspx?d=POP&f=tableCode%3a1">United Nations Population Division data for Burundi</a>
- <em>Master Sheet U5MR Final.csv</em>: <a href="https://data.un.org/Data.aspx?d=POP&f=tableCode%3a1">United Nations Population Division data for Burkina Faso</a>
- <em>Master Sheet UNPD World.csv</em>: <a href="https://data.un.org/Data.aspx?d=POP&f=tableCode%3a1">United Nations Population Division data for China</a>
- <em>Master Sheet UNPD.csv</em>: <a href="https://data.un.org/Data.aspx?d=POP&f=tableCode%3a1">United Nations Population Division data for Republic of Congo</a>

## R libraries
<code>dplyr</code>, <code>ggplot2</code>, <code>ggthemes</code>, <code>readr</code>, <code>tidyr</code>
<br>
<br>

<p><a href="https://www.flinders.edu.au"><img align="bottom-left" src="www/Flinders_University_Logo_Stacked_RGB_Master.jpg" alt="Flinders University" height="50" style="margin-top: 20px"></a> &nbsp; <a href="https://globalecologyflinders.com"><img align="bottom-left" src="www/GEL Logo Kaurna New Transp.png" alt="GEL" height="50" style="margin-top: 20px"></a> &nbsp; &nbsp; <a href="https://www.uwa.edu.au/"><img align="bottom-left" src="www/uwa2.png" alt="UWA" width="100" style="margin-top: 20px"></a> &nbsp; &nbsp; <a href="https://futurechildhealth.org"><img align="bottom-left" src="www/FCHlogoFinaltransp.png" alt="FCH" width="130" style="margin-top: 20px"></a>  &nbsp; <a href="https://www.uwa.edu.au/"><img align="bottom-left" src="www/uwa2.png" alt="UWA" width="100" style="margin-top: 20px"></a> &nbsp; &nbsp; <a href="https://futurechildhealth.org"><img align="bottom-left" src="www/HTHCSPHtransp.png" alt="Harvard T.H. Chan" width="100" style="margin-top: 20px"></a></p>
