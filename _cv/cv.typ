// ============================================================================
// cv.typ — the content of Carlisle Rainey's CV.
//
// This file holds content only. All formatting lives in cv-style.typ.
// Build with:  typst compile _cv/cv.typ _cv/cv.pdf
//
// Helpers available here (defined in cv-style.typ):
//   #doi(url) #pdf(url) #errata(url) #website(url) #dropbox(url)
//   #materials(url)      -> a bracketed hyperlink, e.g. [DOI]
//   #lk("label", url)    -> any other bracketed hyperlink
//   #note[...]           -> small italic annotation line
//   #plain[...]          -> an un-bulleted paragraph inside a section
//   #institution[...]    -> a school name above a list of degrees
//   #subhead[...]        -> a light italic sub-heading (Current / Past)
//   #address-block[..][..], #teaching(..)
// ============================================================================

#import "cv-style.typ": *

#show: cv.with(
  name: "Carlisle Rainey",
  title: "Curriculum Vitae",
  footer-name: "Rainey",
)

= Address

#address-block[
  Carlisle Rainey \
  Department of Political Science \
  Florida State University \
  600 W. College Avenue Tallahassee, FL 32306 \
  #link("mailto:crainey@fsu.edu")[crainey\@fsu.edu]
][
  GitHub: #link("https://github.com/carlislerainey")[carlislerainey] \
  ORCID: #link("https://orcid.org/0000-0002-8728-4696")[0000-0002-8728-4696] \
  Google Scholar: #link("https://scholar.google.com/citations?user=otXLf3wAAAAJ")[otXLf3wAAAAJ]
]

= Academic Appointments

#subhead[Current]

- Professor, Department of Political Science, Florida State University, 2026 – present.

#subhead[Past]

- Associate Professor, Department of Political Science, Florida State University, 2018 – 2026.
- Assistant Professor, Department of Political Science, Texas A&M University, 2015 – 2018.
- Assistant Professor, Department of Political Science, University at Buffalo, SUNY, 2013 – 2015.

= Administrative Appointments

#subhead[Current]

- Associate Editor, _Political Analysis_, 2026 – present.
- Director of Graduate Studies, Department of Political Science, Florida State University, 2026 – present.

#subhead[Past]

- Director, Research Intensive Bachelor's Certificate Program in Political Science, Florida State University, 2021 – 2026.

= Education

#institution[Florida State University]
- Ph.D., Political Science, 2013.
- M.S., Mathematical Statistics, 2012.
- M.S., Political Science, 2009.

#institution[Georgia Southwestern State University]
- B.S., Political Science and Sociology, 2008.

= Papers

#note[
  For the most recent list, see
  #link("https://www.carlislerainey.com/research/")[carlislerainey.com/research/] or
  #link("https://scholar.google.com/citations?user=otXLf3wAAAAJ")[Google Scholar].
]

== Working Papers, R&Rs, and Conditionally Accepted Papers

- "Power Rules: Practical Advice for Computing Power (and Automating with Pilot Data)." Revised version conditionally accepted at _Political Analysis_. #doi("https://doi.org/10.31219/osf.io/5am9q")

- "Merely Asking: A Replication of Ahler and Sood (2018)." Invitation to revise and resubmit at _Journal of Politics_. #dropbox("https://www.dropbox.com/scl/fi/cldf7etwj4d8d1nfkx3kl/cutler_pietryka_rainey_merely_asking.pdf?rlkey=iztyo4d92z841qbjlvezky6un&st=2rzv98ii&e=1&dl=0")

- "Overt Consequences of Covert Actions: Success, Failure, and Voters' Preferences for Legislative Oversight." Invitation to revise and resubmit at _Journal of Experimental Political Science_. #doi("https://doi.org/10.31235/osf.io/9p5h8")

- "Generation Effects on Americans' Symbolic Ideology and Attitudes Toward the Economic Role of Government." #doi("https://osf.io/preprints/socarxiv/ck7de/")

- "Lurking for the Common Good: Does Observing Civil Cross-Party Dialogue on Social Media Reduce Affective Polarization? A Registered Report" #pdf("https://www.carlislerainey.com/papers/lurking.pdf")

== Forthcoming Papers

- Rainey, Carlisle, and Harley Roe. 2025. "The Data Availability Policies of Political Science Journals." Accepted at _Scientific Data_. #doi("https://osf.io/preprints/socarxiv/df2ya")

- Rainey, Carlisle, Harley Roe, Qing Wang, and Nick Dietrich. 2025. "The Dissent Score: Using Events Data to Measure Dissent." Accepted at the _Journal of Peace Research_. #doi("https://osf.io/preprints/socarxiv/dw7np") #website("https://www.carlislerainey.com/dissent-scores/")

== Published Papers

- Rainey, Carlisle and Robert A. Jackson. 2026. "Does Support for the Legal Right to an Abortion Differ Across Generations in the United States?" _PLOS One_ 21(3): e0341223. #doi("https://doi.org/10.1371/journal.pone.0341223")

- Rainey, Carlisle. 2025. "Use and Misuse of a Fast Approximation: Not a Criticism, but a Caution." _Meta-Psychology_ 9: 1–3. #doi("https://doi.org/10.15626/MP.2024.4216") (A peer-reviewed commentary on #link("https://osf.io/preprints/metaarxiv/knjea")[this paper].)

- Clifford, Scott and Carlisle Rainey. 2025. "The Limits (and Strengths) of Single-Topic Experiments." _Political Analysis_ 33(2): 164–170. #doi("https://doi.org/10.1017/pan.2024.20")

- Rainey, Carlisle, Harley Roe, Qing Wang, and Hao Zhou. 2025. "Data and Code Availability in Political Science Publications from 1995 to 2022." _PS: Political Science and Politics_ 58(2): 339–345. #doi("https://doi.org/10.1017/S1049096524001276")

- Clifford, Scott and Carlisle Rainey. 2024. "Estimators for Topic-Sampling Designs." _Political Analysis_ 32(4): 431–444. #doi("https://doi.org/10.1017/pan.2024.1")

- Rainey, Carlisle. 2024. "A Careful Consideration of CLARIFY: Simulation-Induced Bias in Point Estimates of Quantities of Interest." _Political Science Research and Methods_ 12(3): 614–623. #doi("https://doi.org/10.1017/psrm.2023.8") #errata("https://doi.org/10.1017/psrm.2023.25")

- Rainey, Carlisle. 2024. "Hypothesis Tests Under Separation." _Political Analysis_ 32(2): 172–185. #doi("https://doi.org/10.1017/pan.2023.28")

- Clifford, Scott, Thomas J. Leeper, and Carlisle Rainey. 2024. "Generalizing Survey Experiments Using Topic Sampling: An Application to Party Cues." _Political Behavior_ 46(2): 1233–1256. #doi("https://doi.org/10.1007/s11109-023-09870-1") #pdf("https://www.carlislerainey.com/papers/gte.pdf")

- Rainey, Carlisle and Kelly McCaskey. 2021. "Estimating Logit Models with Small Samples." _Political Science Research and Methods_ 9(3): 549–564. #doi("https://doi.org/10.1017/psrm.2021.9")

- Baissa, Daniel K. and Carlisle Rainey. 2020. "When BLUE Is Not Best: Non-Normal Errors and the Linear Model." _Political Science Research and Methods_ 8(1): 136–148. #doi("https://doi.org/10.1017/psrm.2018.34") #pdf("https://www.carlislerainey.com/papers/heavy-tails.pdf")

- Rainey, Carlisle and Robert A. Jackson. 2018. "Unreliable Inferences about Unobserved Processes: A Critique of Partial Observability Models." _Political Science Research and Methods_ 6(2): 381–391. #doi("https://doi.org/10.1017/psrm.2017.3") #pdf("https://www.carlislerainey.com/papers/unreliable.pdf")

- Rainey, Carlisle. 2017. "Transformation-Induced Bias: Unbiased Coefficients Do Not Imply Unbiased Quantities of Interest." _Political Analysis_ 25(3): 402–409. #doi("https://doi.org/10.1017/pan.2017.11") #pdf("https://www.carlislerainey.com/papers/bias.pdf")

- Rainey, Carlisle. 2016. "Dealing with Separation in Logistic Regression Models." _Political Analysis_. 24(3): 339–355. #doi("https://doi.org/10.1093/pan/mpw014") #pdf("https://www.carlislerainey.com/papers/separation.pdf")

- Rainey, Carlisle. 2016. "Compression and Conditional Effects: A Product Term Is Essential When Using Logistic Regression to Test for Interaction." _Political Science Research and Methods_. 4(3): 621–639. #doi("https://doi.org/10.1017/psrm.2015.59") #pdf("https://www.carlislerainey.com/papers/compress.pdf")

- Rainey, Carlisle. 2016. "Does District Magnitude Matter? The Case of Taiwan." _Electoral Studies_. 41: 202–212. #doi("https://doi.org/10.1016/j.electstud.2015.08.009") #pdf("https://www.carlislerainey.com/papers/taiwan.pdf")

- McCaskey, Kelly and Carlisle Rainey. 2015. "Substantive Importance and the Veil of Statistical Significance." _Statistics, Politics, and Policy_ 6(1–2): 77–96. #doi("https://doi.org/10.1515/spp-2015-0001") #pdf("https://www.carlislerainey.com/papers/meaningful.pdf")

- Clifford, Scott, Jennifer Jerit, Carlisle Rainey, and Matt Motyl. 2015. "Moral Concerns and Policy Attitudes: Investigating the Influence of Elite Rhetoric." _Political Communication_. 32(2): 229–248. #doi("https://doi.org/10.1080/10584609.2014.944320") #pdf("https://www.carlislerainey.com/papers/rhetoric.pdf")

- Rainey, Carlisle. 2015. "Strategic Mobilization: Why Proportional Representation Decreases Voter Mobilization." _Electoral Studies_. 37(1): 86–98. #doi("https://doi.org/10.1016/j.electstud.2014.10.008") #pdf("https://www.carlislerainey.com/papers/stratmob.pdf")

- Rainey, Carlisle. 2014. "Arguing for a Negligible Effect." _American Journal of Political Science_ 58(4): 1083–1091. #doi("https://doi.org/10.1111/ajps.12102") #pdf("https://www.carlislerainey.com/papers/nme.pdf")

- Barabas, Jason, Jennifer Jerit, William Pollock, and Carlisle Rainey. 2014. "The Question(s) of Political Knowledge." _American Political Science Review_ 108(4): 840–855. #doi("https://doi.org/10.1017/S0003055414000392") #pdf("https://www.carlislerainey.com/papers/quadrants.pdf")

- Barrilleaux, Charles and Carlisle Rainey. 2014. "The Politics of Need: Examining Governors' Decisions to Oppose the 'Obamacare' Medicaid Expansion." _State Politics and Policy Quarterly_. 14(4): 437–460. #doi("https://doi.org/10.1177/1532440014561644") #pdf("https://www.carlislerainey.com/papers/need.pdf")

= Recent Talks

#note[Note: These are presentations from the past three years for which I was the primary presenter.]

- "Power Rules." 2025. SPSA. #materials("https://www.carlislerainey.com/talks/2025-01-11-power-rules-spsa/")
- "Power Rules." 2024. University of Georgia. #materials("https://www.carlislerainey.com/talks/2024-10-01-power-rules-uga/")
- "Dissent Scores." 2024. Connected Politics Lab at University College Dublin. #materials("https://www.carlislerainey.com/talks/2024-03-06-dissent-scores/")
- "Topic-Sampling." 2024. EPOVB. #materials("https://www.carlislerainey.com/talks/2024-03-02-topic-sampling/")
- "Topic-Sampling." 2024. SPSA. #materials("https://www.carlislerainey.com/talks/2024-topic-sampling/")
- "Topic-Sampling." 2023. EPOVB. #materials("https://www.carlislerainey.com/talks/2023-topic-sampling/")

= Awards

- Faculty Research Award, Mid-Career, College of Social Sciences and Public Policy, Florida State University, 2026.
- Reviewer of the Year, _Political Analysis_, 2024.

= Service

== Discipline

- Reviewer for Cambridge University Press, Sage, _American Political Science Review; American Journal of Political Science; Journal of Politics; Political Analysis; British Journal of Political Science; International Organization; Comparative Politics; Comparative Political Studies; International Studies Quarterly; Journal of Elections, Public Opinion, and Parties; Journal of Official Statistics; Journal of Peace Research; Meta-Psychology; Observational Studies, Party Politics; Political Behavior; Political Research Quarterly; Political Science Research and Methods; PS: Political Science and Politics; Public Opinion Quarterly; The R Journal; Research and Politics; Royal Society Open Science; State and Local Government Review; State Politics and Policy Quarterly._

- Panelist for National Science Foundation, 2021–2024.

- Head of the Political Methodology Section for the 2026 Annual Meeting of the Southern Political Science Association

- Co-chair of the Political Methodology Section for the 2026 Annual Meeting of the American Political Science Association

== University

- FSU Council on Research and Creativity COFRS-SRS Review Panel, 2023.

== College

- College of Social Sciences and Public Policy Curriculum Committee, 2025–present.

== Department

#plain[
  Director of Research Intensive Bachelor's Certificate Program (2021–2026), Executive Committee
  (2019–2021; 2022–2024), Graduate Studies Committee (2019–2026), Methods Field Coordinator
  (2019–2026), Political Behavior Faculty Search Committee (2026–2027), Race and Ethnic Politics
  Faculty Search Committee (2022–2023), Promotion and Tenure Committee (2018–2022)
]

#plain[
  Prior to FSU: Head's Advisory Committee (2017–18, TAMU), Graduate Studies Committee (2013–15, UB),
  Methodology Search Committee (2014–15, UB), American Politics Search Committee (2013–14, UB),
  Methodology Search Committee (2012–13, FSU).
]

= Teaching

#teaching(
  (
    school: "FSU",
    undergraduate: [Research Methods; Applied Research; Empirical Democratic Theory.],
    graduate: [Probability and Inference; Probability Models/Maximum Likelihood.],
  ),
  (
    school: "TAMU",
    undergraduate: [Research Methods.],
    graduate: [Advanced Probability and Inference.],
  ),
  (
    school: "UB",
    undergraduate: [American Politics; Comparative Politics; Research Methods.],
    graduate: [Linear Model; Probability Models/Maximum Likelihood.],
  ),
)
