CREATE TABLE positions (
  position_id SERIAL PRIMARY KEY,
  title VARCHAR (25) NOT NULL UNIQUE
);

CREATE TABLE employees (
  employee_id SERIAL PRIMARY KEY,
  first_name VARCHAR (25) NOT NULL,
  last_name VARCHAR (25) NOT NULL,
  position_id INT REFERENCES positions (position_id) ON DELETE SET NULL,
  created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
  updated TIMESTAMP WITH TIME ZONE
);

CREATE TABLE vacations (
  vacation_id SERIAL PRIMARY KEY,
  employee_id INT REFERENCES employees (employee_id) ON DELETE CASCADE,
  since DATE NOT NULL,
  until DATE NOT NULL,
  created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
  updated TIMESTAMP WITH TIME ZONE
);

INSERT INTO positions(title)
    VALUES
      ('developer'),
      ('designer'),
      ('manager'),
      ('accountant');
