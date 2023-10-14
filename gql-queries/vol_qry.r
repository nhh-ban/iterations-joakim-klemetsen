# Function returning hourly traffic volumes for a given station (id) for a 
# given time interval [from, to].
vol_qry <- function(id, from, to){
  trafficData(trafficRegistrationPointId: id) {
    volume {
      byHour(from: from, to: to) {
        edges {
          node {
            from
            to
            total {
              volumeNumbers {
                volume
              }
            }
          }
        }
      }
    }
  }
}