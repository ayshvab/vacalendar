### Requirements

* Mac / Linux
* Docker
* Docker Compose

### Run
```bash
$ git clone git@github.com:antonshwab/vacalendar.git
$ cd vacalendar
$ make up

sbt:vacalendar> ~reStart
```

### Test
Tests require running Docker
```bash
$ make test
```

### Authentication
All the endpoints require a Bearer JWT Authorization token in the headers, so make sure you have it when performing a request.

It should follow the form: `Authorization: Bearer <your_access_token>`

To generate access token:
```bash
sbt:vacalendar> console
sbt:vacalendar> com.vacalendar.endpoints.auth.ApiTokenGenerator.gen().unsafeRunSync()
Generating API Token
eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJkZXZfYXBpX2tleSIsImp0aSI6ImU2NGY1Nzc2MWNlYmMwMGI1ZjRlOWVkNmU2ZTQ4YTQxNTMxY2NiMjM4N2E2NTNmZGMwMTI0MmY1ZTdhZjNhNDIifQ.8l4NagTNxpcpLe0L8yibsSPczywpgZWuWOgGo-uMMVM
```

