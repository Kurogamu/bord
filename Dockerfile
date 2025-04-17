FROM node:alpine

RUN apk add --update --no-cache openjdk11
RUN npm install -g shadow-cljs 

WORKDIR /app
COPY package.json .
RUN npm install

COPY . .

RUN npm run cljs-build
RUN npm run css-build
RUN npm run lib-build
