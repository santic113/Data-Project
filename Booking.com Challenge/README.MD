## **Challenge Summary & Information**

### **Challenge**
<font size="4"> The goal of this challenge is to use a dataset based on millions of real anonymized accommodation reservations to come up with a strategy for making the best recommendation for their next destination in real-time. </font>
<br><br><br>

### **Dataset**
* <font size="4"> The training dataset consists of over a million of anonymized hotel reservations, based on real data, with the following features: </font>
* <font size="4">  **user_id** - User ID
* **checkin** - Reservation check-in date
* **checkout** - Reservation check-out date- created_date - Date when the reservation was made
* **affiliate_id** - An anonymized ID of affiliate channels where the booker came from (e.g. direct, some third party referrals, paid search engine, etc.)
* **device_class** - desktop/mobile
* **booker_country** - Country from which the reservation was made (anonymized)
* **hotel_country** - Country of the hotel (anonymized)
* **city_id** - city_id of the hotel's city (anonymized)
* **utrip_id** - Unique identification of user's trip (a group of multi-destinations bookings within the same trip).
* **Note** -Each reservation is a part of a customer's trip (identified by utrip_id) which includes at least 4 consecutive reservations. The check-out date of a reservation is the check-in date of the following reservation in their trip.
The evaluation dataset is constructed similarly, however the city_id of the final reservation of each trip is concealed and requires a prediction. </font>
<br><br><br>

### **Evaluation & Goal**
<font size="4"> The goal of the challenge is to predict (and recommend) the final city (city_id) of each trip (utrip_id). We will evaluate the quality of the predictions based on the top four recommended cities for each trip by using Precision@4 metric (4 representing the four suggestion slots at Booking.com website). When the true city is one of the top 4 suggestions (regardless of the order), it is considered correct. </font>


## Structure of Project & Exploration

### **Quotes on Data & Analytics**
> “Data are just summaries of thousands of stories – tell a few of those stories to help make the data meaningful.” — Chip & Dan Heath

> "A data scientist is someone who can obtain, scrub, explore, model, and interpret data, blending hacking, statistics, and machine learning. Data scientists not only are adept at working with data, but appreciate data itself as a first-class product.” – Hillary Mason, founder, Fast Forward Labs.

> “Information is the oil of the 21st century, and analytics is the combustion engine.” – Peter Sondergaard, senior vice president, Gartner Research.
<br><br><br>

### **What are my aims with this exploration?**  
* <font size="4"> Distributions </font>
* <font size="4"> Basic Descriptive Metrics </font>
* <font size="4"> Correlations/ Relationships </font>
* <font size="4"> Identifying Outliers or Odd Data </font>
* <font size="4"> Seasonality/Time Series</font>
* <font size="4"> Creation of New Columns for Feature Engineering </font>
* <font size="4"> Outlier Detection </font>
* <font size="4"> Volume </font>
<br><br><br>

### **Structure of the Exploration**
*  <font size="4"> **Questions**:  Questions will be asked previous to the visualization to make sure the visualizations shown in this project are insightful.</font>

* <font size="4"> **Summary**: After each section I will provide a summary to understand what we got from the visualizations

* <font size="4"> **Observations/Comments**: Any observations I made with a given metric or visual or comments I have about a given summary of data
<br><br><br>

### **Objective of the Exploration**
* <font size="4"> **Observations** - What can we learn from this deep dive that we didn't know before from our inital observations of the dataset. </font>

* <font size="4"> **Understanding Current Dataset** - The intention of this exploration is to also understand the booking.com dataset and how a customer travels using booking.com. </font>

* <font size="4"> **Modeling** - Creating a model to predict (and recommend) the final city (city_id) of each trip (utrip_id) </font>
<br><br><br>

### **Other Thoughts**
* <font size="4"> Analytics teams often make the trade-off between speed and accuracy. The trade-off often
results in solutions that are challenging to interpret and deploy for the wider organization. A natural drawback
of scrappy or agile analytics approaches for systematic solutions are wide gaps or 'blind spots' in analysis and
unstable/brittle tools, often leading to sub-optimal outcomes. This write up serves to document  and compare past business cases, analytics methodologies
and learnings in order for the reader to gain a good understanding of historical efforts to date. </font>
