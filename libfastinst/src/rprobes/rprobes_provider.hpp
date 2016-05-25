
typedef std::unordered_map<Range, ControlTransfer> ControlTransferMap;

class RprobesProvider : public ProbeProvider {
  public:
    RprobesProvider(Callback cb) : ProbeProvider(cb) {
    }

  private:
    ControlTransferMap control_transfers;

}; 
