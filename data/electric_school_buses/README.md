# README `/electric_school_buses`

## 🚌 Electric School Bus Price Tracker — State-Level Base Prices

This dataset provides **state-level base prices** for electric school buses, compiled from publicly available state contracts and procurement sources. It accompanies WRI's [Electric School Bus U.S. Market Study and Buyer’s Guide](https://electricschoolbusinitiative.org/electric-school-bus-market-study-and-electric-school-bus-us-buyers-guide). Developed by ElectricSchoolBusInitiative.org.

*Last updated: December 2022*

------------------------------------------------------------------------

## `buses.csv`

Each row represents a **base price listing for a specific electric school bus model** available through a state's procurement system.

------------------------------------------------------------------------

### 📄 Column Descriptions

| Column | Description |
|--------------------|----------------------------------------------------|
| `source_type` | Type of procurement source (e.g., `"State Contract"`) |
| `purchase_year` | Year for which the listed price is valid |
| `bus_manufacturer` | Manufacturer of the electric school bus |
| `bus_model` | Model name or identifier (may be missing) |
| `bus_type` | Bus type: A (small), C (standard), D (large) |
| `seating_capacity` | Number of seats |
| `base_price` | Listed base price of the bus in USD |
| `special_needs_bus` | Whether the bus is designated for special needs use (`Yes`, `No`, or `NA`) |
| `state` | U.S. state abbreviation (e.g., `"AR"`) |
| `vehicle_dealer` | Name of the vehicle dealer (if applicable) |
| `source` | Name of the procurement agency or document source |
| `source_url` | URL to the publicly available procurement document |
| `date_published_or_updated` | Publication or update date of the source document (Excel serial format) |

------------------------------------------------------------------------

### ⚠️ Notes and Considerations

-   **Base Prices Only**: Prices reflect *base models* and do not include taxes, delivery, optional features, or multi-bus discounts.
-   **Purchase Year ≠ Model Year**: Data reflects the year the price was listed, not necessarily the vehicle's model year.
-   **Not Comprehensive**: Dataset includes only available prices from state-level sources; it does not cover every electric bus on the market.
-   **Special Needs Field**: May be `"N/A"` when states do not list special needs buses separately.

------------------------------------------------------------------------

### 🚸 Bus Type Definitions

| Type | Description                           |
|------|---------------------------------------|
| A    | Small bus, typically \< 36 passengers |
| C    | Standard bus, \~40–83 passengers      |
| D    | Largest buses, up to 90 passengers    |

------------------------------------------------------------------------

### 🗂 Source Information

Data was collected from state procurement agencies. If a price listing was not found via desk research, WRI researchers contacted state officials directly to verify availability.

------------------------------------------------------------------------

### 📅 Updates

This dataset is updated annually in conjunction with WRI's market study. Future versions may incorporate:

-   Dealer-level pricing
-   District-level purchases
-   More recent purchase years

For questions or data requests, contact: **Phillip Burgoyne-Allen**\
📧 [phillip.burgoyne-allen\@wri.org](mailto:phillip.burgoyne-allen@wri.org){.email}

------------------------------------------------------------------------

### 📚 Learn More

-   🔗 [Electric School Bus Initiative](https://electricschoolbusinitiative.org/)
-   📘 [Market Study & Buyer’s Guide](https://electricschoolbusinitiative.org/electric-school-bus-market-study-and-electric-school-bus-us-buyers-guide)

------------------------------------------------------------------------

## 🧪 View in R

``` r
library(readr)
read_rds("data/risk_index/electric_bus_prices.rds") %>% head()
```
