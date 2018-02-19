FROM swipl

MAINTAINER cjmungall@lbl.gov

COPY . /app/biomake

ENV PATH="$PATH:/app/biomake/bin"

CMD ["biomake", "-h"]
