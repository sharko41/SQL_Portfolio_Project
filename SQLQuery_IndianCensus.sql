--Verify the data imported

select * from Data1;

select * from Data2;

-- drop the extra columns

alter TABLE data1
drop COLUMN F7,F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18, F19, F20, F21, F22, F23, F24, F25
, F26;

-- drop both tables

drop table Data1;
drop table data2;

-- check the count of rows

select COUNT(*) from Data1;

select COUNT(*) from Data2;

-- dataset for Jharkhand and bihar

select * from data2
select * from data1
where state IN ('Jharkhand', 'Bihar');

-- total population of india

select SUM(population) AS Total_India_Population from data2

-- avg growth of population of india

select round(AVG(growth)*100,2) Avg_Growth from data1;

-- avg growth of population By states of india

select State, round(AVG(growth)*100,2) Avg_Growth from data1
Group by state;

-- avg sex ratio of population By states of india

select State, round(AVG(sex_ratio),0) Avg_Sexratio from data1
Group by state 
order by avg_sexratio desc;

-- avg Literacy rate of population By states of india

select State, round(AVG(literacy),0) Avg_literacy from data1
Group by state 
order by Avg_literacy desc;

select State, round(AVG(literacy),0) Avg_literacy from data1
Group by state 
having round(AVG(literacy),0) > 90
order by Avg_literacy desc;

-- top 3 states showing highest growth ratio

select Top 3 State, round(AVG(growth)*100,2) Avg_Growth from data1
Group by state
order by Avg_Growth desc;

select State, round(AVG(growth)*100,2) Avg_Growth from data1
Group by state
order by Avg_Growth desc
Offset 0 rows
Fetch next 3 rows only;

-- top 3 states showing lowest growth ratio & lowest sex ratio

select Top 3 State, round(AVG(growth)*100,2) Avg_Growth from data1
Group by state
order by Avg_Growth asc;

select State, round(AVG(growth)*100,2) Avg_Growth from data1
Group by state
order by Avg_Growth asc
Offset 0 rows
Fetch next 3 rows only;

select Top 3 State, round(AVG(sex_ratio),0) Avg_Sexratio from data1
Group by state 
order by avg_sexratio asc;

select State, round(AVG(sex_ratio),0) Avg_Sexratio from data1
Group by state 
order by avg_sexratio asc
Offset 0 rows
Fetch next 3 rows only;

-- top and bottom states in literacy rate

drop table if exists d1
create table d1 
	( state nvarchar (255),
		bottomstate float
	)
insert into d1 
select State, round(AVG(literacy),0) Avg_literacy from data1
Group by state 
order by Avg_literacy asc;

select Top 3 * from d1 order by bottomstate asc;

drop table if exists d2
create table d2 
	( state nvarchar (255),
		topstate float
	)
insert into d2 
select State, round(AVG(literacy),0) Avg_literacy from data1
Group by state 
order by Avg_literacy desc;

select Top 3 * from d2 order by topstate desc;

-- union operator

select * from(
select Top 3 * from d2 order by topstate desc) a
union
select * from(
select Top 3 * from d1 order by bottomstate asc) b;

--States starting/ending with different letter 

select distinct state from data1 where state like 'a%';

select distinct state from data1 where state like 'a%' or state like 'b%';

select distinct state from data1 where state like 'J%' AND state like '%d';

-- joining tables

select * from Data1;

select * from Data2;

select d.district, d.state, d.sex_ratio, b.population
from data1 d
inner join data2 b
On d.district = b.district;

select c.district, c.state, round(c.population/(c.sex_ratio+1),0) Males, round((c.population*c.sex_ratio)/(c.sex_ratio+1),0) Females from
(select d.district, d.state, d.sex_ratio/1000 sex_ratio, b.population
from data1 d
inner join data2 b
On d.district = b.district)c;

--statewise male female population

select e.state, SUM(males) Male_population, SUM(females) Female_population from (
select c.district, c.state, round(c.population/(c.sex_ratio+1),0) Males, round((c.population*c.sex_ratio)/(c.sex_ratio+1),0) Females from
(select d.district, d.state, d.sex_ratio/1000 sex_ratio, b.population
from data1 d
inner join data2 b
On d.district = b.district)c)e
group by e.state;


-- literacy rate 

select d.district, d.state, d.literacy/100 literacy_ratio, b.population
from data1 d
inner join data2 b
On d.district = b.district


select f.district, f.state, round(f.literacy_ratio*f.population,0) Literate_people, round((1-f.literacy_ratio)*f.population,0) illiterate_people from
(
select d.district, d.state, d.literacy/100 literacy_ratio, b.population
from data1 d
inner join data2 b 
On d.district = b.district
)f;

--Literacy by states

select b.state, SUM(Literate_people) Total_Literate, SUM(illiterate_people) Total_Illiterate from (
select f.district, f.state, round(f.literacy_ratio*f.population,0) Literate_people, round((1-f.literacy_ratio)*f.population,0) illiterate_people from
(
select d.district, d.state, d.literacy/100 literacy_ratio, b.population
from data1 d
inner join data2 b 
On d.district = b.district
)f)b
group by b.state;


--population in previous census


select a.district, a.state, round(a.population/(1+a.growth_ratio),0) previous_population, a.population current_population from 
(
select d.district, d.state, d.growth growth_ratio, b.population
from data1 d
inner join data2 b 
On d.district = b.district
) a ;


--population by states

select b.state, sum(b.previous_population) Total_Previous_Pop, sum(b.current_population) Total_Current_Pop from	
(
select a.district, a.state, round(a.population/(1+a.growth_ratio),0) previous_population, a.population current_population from 
(
select d.district, d.state, d.growth growth_ratio, b.population
from data1 d
inner join data2 b 
On d.district = b.district
) a )b
group by b.state;


--Total population comparision

select SUM(m.Total_Previous_Pop) Prev_Pop, SUM(m.Total_Current_Pop) Curr_Pop from
(
select b.state, sum(b.previous_population) Total_Previous_Pop, sum(b.current_population) Total_Current_Pop from	
(
select a.district, a.state, round(a.population/(1+a.growth_ratio),0) previous_population, a.population current_population from 
(
select d.district, d.state, d.growth growth_ratio, b.population
from data1 d
inner join data2 b 
On d.district = b.district
) a )b
group by b.state)m;


--population VS Area



select q.*, r.* from (

Select '1' as keyy,n. * from (
select SUM(m.Total_Previous_Pop) Prev_Pop, SUM(m.Total_Current_Pop) Curr_Pop from
(
select b.state, sum(b.previous_population) Total_Previous_Pop, sum(b.current_population) Total_Current_Pop from	
(
select a.district, a.state, round(a.population/(1+a.growth_ratio),0) previous_population, a.population current_population from 
(
select d.district, d.state, d.growth growth_ratio, b.population
from data1 d
inner join data2 b 
On d.district = b.district
) a )b
group by b.state)m)n ) q 

inner join (


select '1' as keyy,A. * from
(
select SUM(Area_km2) Total_Area  from data2
) A
) r 

On q.keyy = r.keyy ;



-- window functions ( top 3 districts from each state with highest literacy rate )

select * from data1;


select district, state, literacy, rank() over(partition by state order by literacy desc ) rnk from data1
order by state ;

select a.* from (
select district, state, literacy, rank() over(partition by state order by literacy desc ) rnk from data1 ) a

where a.rnk in (1,2,3)  order by state ;
