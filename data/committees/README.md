
# README `/committees`

## Distaster Recovery Committees Datasets

Citations:

- Fraser, T., Aldrich, D. P., Small, A., & Littlejohn, A. (2021). In the hands of a few: Disaster recovery committee networks. Journal of environmental management, 280, 111643.
- Fraser, T., Aldrich, D. P., & Small, A. (2021). Seawalls or social recovery? The role of policy networks and design in disaster recovery. Global Environmental Change, 70, 102342.

### `committees.csv` - Disaster Recovery Committee Traits

| Variable             | Type      | Description                               | Example Values                    |
| -------------------- | --------- | ----------------------------------------- | --------------------------------- |
| `name`               | character | Unique committee ID                       | "committee\_1", "committee\_2"    |
| `type`               | logical   | Whether entry is a committee (TRUE/FALSE) | TRUE, TRUE                        |
| `committee_type`     | character | Type of committee                         | "Other", "Municipal"              |
| `geography`          | character | Geographic scope                          | "national", "iwate"               |
| `level`              | character | Administrative level                      | "national", "municipal"           |
| `town`               | character | Town or city name                         | "National", "Otsuchi-cho"         |
| `committee_romaji`   | character | Committee name in Romanized Japanese      | "3.11 Shinsai Densho- Kenkyu-kai" |
| `committee_japanese` | character | Committee name in Japanese characters     | "3.11震災伝承研究会", "大槌町復興戦略会議"        |

---


### `members.csv` - Individual Member Attributes

| Variable                        | Type      | Description                                       | Example Values       |
| ------------------------------- | --------- | ------------------------------------------------- | -------------------- |
| `name`                          | character | Unique member ID                                  | "name\_1", "name\_2" |
| `type`                          | logical   | TRUE if committee, FALSE if member                | FALSE, FALSE         |
| `role`                          | character | Role in committee                                 | "member", "chair"    |
| `birth_year`                    | double    | Birth year                                        | 1961, 1949           |
| `age`                           | double    | Age                                               | 58, 70               |
| `age_group`                     | character | Age category                                      | "40-60", "60-80"     |
| `gender`                        | character | Gender                                            | "man", "woman"       |
| `social_org`                    | character | Social organization affiliation                   | "no", "yes"          |
| `influential_citizen`           | character | Influential citizen status                        | "no", "yes"          |
| `politician`                    | character | Politician status                                 | "no", "yes"          |
| `politician_level`              | character | Political level                                   | NA, "municipal"      |
| `politician_role`               | character | Political role                                    | NA, "council member" |
| `politician_party`              | character | Political party                                   | NA, "LDP"            |
| `govt`                          | character | Government affiliation                            | "no", "yes"          |
| `govt_natl`                     | character | National government affiliation                   | "no", "yes"          |
| `govt_pref`                     | character | Prefectural government affiliation                | "no", "yes"          |
| `govt_muni`                     | character | Municipal government affiliation                  | "no", "yes"          |
| `research`                      | character | Research sector affiliation                       | "no", "yes"          |
| `business`                      | character | Business sector affiliation                       | "no", "yes"          |
| `fisheries`                     | character | Fisheries sector affiliation                      | "no", "yes"          |
| `agriculture`                   | character | Agriculture sector affiliation                    | "no", "yes"          |
| `emergency_services`            | character | Emergency services affiliation                    | "no", "yes"          |
| `health_care`                   | character | Health care sector affiliation                    | "no", "yes"          |
| `lawyer`                        | character | Lawyer affiliation                                | "no", "yes"          |
| `religious`                     | character | Religious affiliation                             | "no", "yes"          |
| `media`                         | character | Media sector affiliation                          | "no", "yes"          |
| `outside_FMI_before_3_11`       | character | Outside Fukushima membership before 3.11 disaster | "no", "yes"          |
| `community_participation`       | character | Community participation                           | "no", "yes"          |
| `community_participation_relig` | character | Religious community participation                 | "no", "yes"          |


### `edgelist.csv` - Committee Membership Edgelist

| Variable       | Type      | Description                               | Example Values                 |
| -------------- | --------- | ----------------------------------------- | ------------------------------ |
| `committee_id` | character | Committee unique ID                       | "committee\_1", "committee\_2" |
| `member_id`    | character | Member unique ID                          | "name\_1", "name\_10"          |
| `weight`       | double    | Weight or strength of membership relation | 1, 1                           |

---

**Edges (749 rows, 3 columns):**

| Variable | Type    | Description                              | Example Values |
| -------- | ------- | ---------------------------------------- | -------------- |
| `from`   | integer | Node ID for source (committee node)      | 1, 10          |
| `to`     | integer | Node ID for target (member node)         | 40, 194        |
| `weight` | double  | Weight of the edge (membership strength) | 1, 1           |

---

### `graph_bipartite.rds` - Bipartite Graph Data of Committees and Members

**Nodes (695 rows, 35 columns):**

| Variable                                      | Type      | Description                                       | Example Values                       |
| --------------------------------------------- | --------- | ------------------------------------------------- | ------------------------------------ |
| `name`                                        | character | Node identifier (committee or member)             | "committee\_1", "name\_1"            |
| `type`                                        | logical   | TRUE = committee, FALSE = member                  | TRUE, FALSE                          |
| `committee_type`                              | character | Committee type (for committees only)              | "Municipal", "Other"                 |
| `geography`                                   | character | Geographic scope                                  | "iwate", "national"                  |
| `level`                                       | character | Administrative level                              | "municipal", "national"              |
| `town`                                        | character | Town or city name                                 | "Ofunato-shi", "National"            |
| `committee_romaji`                            | character | Committee name in Romanized Japanese              | "O-tsuchi cho- Fukko Senryaku Kaigi" |
| `committee_japanese`                          | character | Committee name in Japanese characters             | "大槌町復興戦略会議"                          |
| `role`                                        | character | Member role in committee (if applicable)          | NA, "member"                         |
| `birth_year`                                  | double    | Birth year of member                              | NA, 1961                             |
| `age`                                         | double    | Age of member                                     | NA, 58                               |
| `age_group`                                   | character | Age group category                                | NA, "40-60"                          |
| `gender`                                      | character | Gender                                            | NA, "man"                            |
| `social_org`                                  | character | Member's social organization affiliation          | NA, "no"                             |
| `influential_citizen`                         | character | Whether member is influential citizen             | NA, "yes"                            |
| `politician`                                  | character | Whether member is a politician                    | NA, "no"                             |
| `politician_level`                            | character | Level of political office                         | NA, "municipal"                      |
| `politician_role`                             | character | Role within political office                      | NA, "council member"                 |
| `politician_party`                            | character | Political party affiliation                       | NA, "LDP"                            |
| `govt`, `govt_natl`, `govt_pref`, `govt_muni` | character | Government affiliation at various levels          | NA, "yes"                            |
| `research`                                    | character | Research sector affiliation                       | NA, "no"                             |
| `business`                                    | character | Business sector affiliation                       | NA, "yes"                            |
| `fisheries`                                   | character | Fisheries sector affiliation                      | NA, "no"                             |
| `agriculture`                                 | character | Agriculture sector affiliation                    | NA, "no"                             |
| `emergency_services`                          | character | Emergency services affiliation                    | NA, "no"                             |
| `health_care`                                 | character | Health care sector affiliation                    | NA, "no"                             |
| `lawyer`                                      | character | Lawyer affiliation                                | NA, "no"                             |
| `religious`                                   | character | Religious affiliation                             | NA, "no"                             |
| `media`                                       | character | Media sector affiliation                          | NA, "no"                             |
| `outside_FMI_before_3_11`                     | character | Outside Fukushima membership before 3.11 disaster | NA, "no"                             |
| `community_participation`                     | character | Community participation status                    | NA, "yes"                            |
| `community_participation_relig`               | character | Religious community participation                 | NA, "no"                             |

---
