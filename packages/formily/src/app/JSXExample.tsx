"use client";
import React, { use, useEffect } from "react";
import { createForm } from "@formily/core";
import { FormProvider, Field, createSchemaField } from "@formily/react";
import { Input } from "antd";

const form = createForm();

function TestComponent({ onChange, value, daa }) {
  useEffect(() => {
      onChange(123);
  }, []);

  return (
    <div>
      bar: {value}; dada: {daa}
    </div>
  );
}

const SchemaField = createSchemaField({
  components: {
    Input,
    TestComponent,
  },
});

export default () => {
  return (
    <FormProvider form={form}>
      <Field name="input" component={[Input, { placeholder: "请输入" }]} />
      <SchemaField
        schema={{
          type: "void",
          properties: {
            bar: {
              type: "string",
              "x-component": "Input",
              "x-component-props": {
                placeholder: "请输入",
              },
            },
            test: {
              type: "string",
              "x-component": "TestComponent",
              "x-component-props": {
                daa: 1,
              },
            },
          },
        }}
      ></SchemaField>
    </FormProvider>
  );
};
