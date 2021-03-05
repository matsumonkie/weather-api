WITH weatherId1 AS (
  INSERT INTO weather (id, name) values (gen_random_uuid(), 'sunny') RETURNING id
), weatherId2 AS (
  INSERT INTO weather (id, name) values (gen_random_uuid(), 'cloudy') RETURNING id
), weatherId3 AS (
  INSERT INTO weather (id, name) values (gen_random_uuid(), 'rainy') RETURNING id
), cityId1 AS (
  INSERT INTO city (id, name) values (gen_random_uuid(), 'marseille') RETURNING id
), cityId2 AS (
  INSERT INTO city (id, name) values (gen_random_uuid(), 'nantes') RETURNING id
), cityId3 AS (
  INSERT INTO city (id, name) values (gen_random_uuid(), 'paris') RETURNING id
), city_weather1 AS (
  INSERT INTO city_weather (id, city_id, weather_id) values (gen_random_uuid(), (select id from cityId1), (select id from weatherId1))
), city_weather2 AS (
  INSERT INTO city_weather (id, city_id, weather_id) values (gen_random_uuid(), (select id from cityId2), (select id from weatherId2))
)
  INSERT INTO city_weather (id, city_id, weather_id) values (gen_random_uuid(), (select id from cityId3), (select id from weatherId3))
