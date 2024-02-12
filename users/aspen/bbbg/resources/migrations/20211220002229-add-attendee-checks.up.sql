CREATE TABLE attendee_check (
    "id" UUID PRIMARY KEY NOT NULL DEFAULT uuid_generate_v4(),
    "attendee_id" UUID NOT NULL REFERENCES attendee ("id"),
    "user_id" UUID NOT NULL REFERENCES "public"."user" ("id"),
    "last_dose_at" DATE,
    "checked_at" TIMESTAMP WITHOUT TIME ZONE NOT NULL DEFAULT now()
);
